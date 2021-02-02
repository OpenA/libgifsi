/* optimize.c - Functions to optimize animated GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#include <config.h>
#include "gifsicle.h"
#include "kcolor.h"
#include <assert.h>
#include <string.h>

typedef signed int penalty_t;

typedef struct {
	const unsigned short left, top, width, height, MAX_W, MAX_H;
} Gif_OptBounds;

typedef struct {
	unsigned short left, top, width, height;
	  signed short transparent;
	unsigned int   size, required_color_count;
	unsigned char *needed_colors;
	penalty_t      global_penalty, active_penalty, colormap_penalty;
	Gif_Disposal   disposal;
} Gif_OptData;

/* Colormap containing all colors in the image. May have >256 colors */
static Gif_Colormap *all_colormap;

/* The old global colormap, or a fake one we created if necessary */
static Gif_Colormap *in_global_map;

/* The new global colormap */
static Gif_Colormap *out_global_map;

static unsigned background;

static penalty_t *permuting_sort_values;

typedef enum {
	TColorEmpty = 0,
	TColorReplace,
	TColorRequired,
	TColorLimit = 256
} Gif_TranColor;


/*****
 * SIMPLE HELPERS
 * new and delete optimize data; and colormap_combine; and sorting permutations
 **/
static Gif_OptData *new_opt_data(void)
{
	Gif_OptData *od    = Gif_New(Gif_OptData);
	od->needed_colors  = NULL;
	od->global_penalty = 1;
	return od;
}

/* colormap_add: Ensure that each color in 'src' is represented in 'dst'.
   For each color 'i' in 'src', src->col[i].pixel == some j
   so that GIF_COLOREQ(&src->col[i], &dst->col[j]).
   dst->col[0] is reserved for transparency;
   no source color willbe mapped to it. */

static void colormap_add(const Gif_Colormap *src, Gif_Colormap *dst, kchist *cm_hist)
{
	/* expand dst->col if necessary. This might change dst->col */
	if (dst->ncol + src->ncol >= dst->capacity) {
		dst->capacity *= 2;
		Gif_ReArray(dst->col, Gif_Color, dst->capacity);
	}
	for (int i = 0; i < src->ncol; i++) {
		kchistitem* khi = kchist_add(cm_hist, kc_makegfcng(&src->col[i]), 0);

		if (!khi->count) {
			int j = (khi->count = dst->ncol);
			dst->col[j] = src->col[i];
			dst->col[j].pixel = TColorEmpty;
			dst->ncol++;
		}
		src->col[i].pixel = khi->count;
	}
}


/*****
 * MANIPULATING IMAGE AREAS
 **/
#define get_safe_bounds(area, max_w, max_h) new_opt_bounds(\
	area->left , area->top,\
	area->width, area->height,\
	max_w, max_h)

static Gif_OptBounds new_opt_bounds(
	unsigned short left , unsigned short top,
	unsigned short width, unsigned short height,
	unsigned short max_w, unsigned short max_h
) {
	/* Returns bounds constrained to lie within the screen. */
	bool out_x = left >= max_w,
	     out_y = top  >= max_h;

	Gif_OptBounds ob = {
	  .left   = out_x ? max_w : left,  .MAX_W = max_w,
	  .top    = out_y ? max_h : top,   .MAX_H = max_h,
	  .width  = out_x ? 0 : _MIN(left + width , max_w) - left,
	  .height = out_y ? 0 : _MIN(top  + height, max_h) - top
	};
	return ob;
}


/*****
 * FIND THE SMALLEST BOUNDING RECTANGLE ENCLOSING ALL CHANGES
 *
 * fix_difference_bounds: make sure the image isn't 0x0. */

static void fix_difference_bounds(Gif_OptData *bounds, unsigned short MAX_W, unsigned short MAX_H)
{
	if (bounds->width == 0 || bounds->height == 0) {
		bounds->top    = 0;
		bounds->left   = 0;
		bounds->width  = 1;
		bounds->height = 1;
	}
	/* assert that image lies completely within screen */
	assert(bounds->top  < MAX_H && bounds->left < MAX_W
	    && bounds->top  + bounds->height <= MAX_H
	    && bounds->left + bounds->width  <= MAX_W);
}


/*****
 * CALCULATE OUTPUT GLOBAL COLORMAP
 **/
static void increment_penalties(unsigned char *need, penalty_t *penalty, penalty_t delta)
{
	for (int i = 1; i < all_colormap->ncol; i++) {
		if (need[i] == TColorRequired)
			penalty[i] += delta;
	}
}


/*****
 * CREATE COLOR MAPPING FOR A PARTICULAR IMAGE
 *
 * sort_colormap_permutation_rgb: for canonicalizing local colormaps by
   arranging them in RGB order */

static int colormap_rgb_permutation_sorter(const void *v1, const void *v2)
{
	const Gif_Color *col1 = (const Gif_Color *)v1;
	const Gif_Color *col2 = (const Gif_Color *)v2;
	int value1 = (col1->gfc_red << 16) | (col1->gfc_green << 8) | col1->gfc_blue;
	int value2 = (col2->gfc_red << 16) | (col2->gfc_green << 8) | col2->gfc_blue;
	return value1 - value2;
}


/* prepare_colormap_map: Create and return an array of bytes mapping from
   global pixel values to pixel values for this image. It may add colormap
   cells to 'into'; if there isn't enough room in 'into', it will return 0. It
   sets the 'transparent' field of 'gfi->optdata', but otherwise doesn't
   change or read it at all. */

static unsigned char *
prepare_colormap_map(Gif_Image *gfi, Gif_Colormap *into, unsigned char *need)
{
	int i;
	int is_global = (into == out_global_map);

	int all_ncol = all_colormap->ncol;
	Gif_Color *all_col = all_colormap->col;

	int ncol = into->ncol;
	Gif_Color *col = into->col;

	unsigned char *map = Gif_NewArray(unsigned char, all_ncol);
	unsigned char into_used[256];

	/* keep track of which pixel indices in 'into' have been used; initially,
	   all unused */
	for (i = 0; i < 256; i++)
		into_used[i] = 0;

	/* go over all non-transparent global pixels which MUST appear
	   (need[P] == TColorRequired) and place them in 'into' */
	for (i = 1; i < all_ncol; i++) {
		int val;
		if (need[i] != TColorRequired)
			continue;

		/* fail if a needed pixel isn't in the global map */
		if (is_global) {
			if (ncol <= (val = all_col[i].pixel))
				goto error;
		} else {
			/* always place colors in a local colormap */
			if (ncol == 256)
				goto error;
			val = ncol;
			col[val] = all_col[i];
			col[val].pixel = i;
			ncol++;
		}
		map[i] = val;
		into_used[val] = 1;
	}

	if (!is_global) {
		qsort(col, ncol, sizeof(Gif_Color), colormap_rgb_permutation_sorter);
		for (i = 0; i < ncol; ++i)
			map[col[i].pixel] = i;
	}

	/* now check for transparency */
	gfi->transparent = -1;
	if (need[TColorEmpty]) {
		int transparent = -1;

		/* first, look for an unused index in 'into'. Pick the lowest one: the
		lower transparent index we get, the more likely we can shave a bit off
		min_code_bits later, thus saving space */
		for (i = 0; i < ncol; i++) {
			if (!into_used[i]) {
				transparent = i;
				break;
			}
		}
		/* otherwise, [1.Aug.1999] use a fake slot for the purely transparent
		color. Don't actually enter the transparent color into the colormap --
		we might be able to output a smaller colormap! If there's no room for
		it, give up */
		if (transparent < 0) {
			if (ncol < 256) {
				transparent = ncol;
				/* 1.Aug.1999 - don't increase ncol */
				col[ncol] = all_col[TColorEmpty];
			} else
				goto error;
		}
		/* change mapping */
		map[TColorEmpty] = transparent;
		for (i = 1; i < all_ncol; i++) {
			if (need[i] == TColorReplace)
				map[i] = transparent;
		}
		gfi->transparent = transparent;
	}
	/* If we get here, it worked! Commit state changes (the number of color
		cells in 'into') and return the map. */
	into->ncol = ncol;
	return map;

error:
	/* If we get here, it failed! Return 0 and don't change global state. */
	Gif_DeleteArray(map);
	return 0;
}


/* prepare_colormap: make a colormap up from the image data by fitting any
   used colors into a colormap. Returns a map from global color index to index
   in this image's colormap. May set a local colormap on 'gfi'. */

static unsigned char *prepare_colormap(Gif_Image *gfi, unsigned char *need)
{
	unsigned char *map;

	/* try to map pixel values into the global colormap */
	Gif_DeleteColormap(gfi->local);
	gfi->local = NULL;
	map = prepare_colormap_map(gfi, out_global_map, need);

	if (!map) {
		/* that didn't work; add a local colormap. */
		gfi->local = Gif_NewColormap(0, 256);
		map = prepare_colormap_map(gfi, gfi->local, need);
	}
	return map;
}


/*****
 * INITIALIZATION AND FINALIZATION
 **/
static void init_colormaps(Gif_Stream *gfs)
{
	/* combine colormaps */
	all_colormap = Gif_NewColormap(1, 384);
	all_colormap->col[0].gfc_red   = 255;
	all_colormap->col[0].gfc_green = 255;
	all_colormap->col[0].gfc_blue  = 255;

	in_global_map = gfs->global;
	if (!in_global_map) {
		in_global_map = Gif_NewColormap(256, 256);
		Gif_Color *col = in_global_map->col;
		for (int i = 0; i < 256; i++, col++)
			col->gfc_red = col->gfc_green = col->gfc_blue = i;
	}

	unsigned i, t;
	int first_transparent = -1;
	bool any_globals = false;

	/* Histogram so we can find colors quickly */
	kchist all_colormap_hist;
	kchist_init(&all_colormap_hist);

	for (i = 0; i < gfs->nimages; i++) {
		Gif_Image *gfi = gfs->images[i];
		if (gfi->local)
			colormap_add(gfi->local, all_colormap, &all_colormap_hist);
		else
			any_globals = true;
		if (gfi->transparent >= 0 && first_transparent < 0)
			first_transparent = i;
	}
	if (any_globals)
		colormap_add(in_global_map, all_colormap, &all_colormap_hist);
	kchist_cleanup(&all_colormap_hist);

	/* try and maintain transparency's pixel value */
	if (first_transparent >= 0) {
		Gif_Image *gfi = gfs->images[first_transparent];
		Gif_Colormap *gfcm = gfi->local ? gfi->local : gfs->global;
		all_colormap->col[TColorEmpty] = gfcm->col[gfi->transparent];
	}

	/* find screen_width and screen_height, and clip all images to screen */
	Gif_CalculateScreenSize(gfs, 0);

	/* Screen width and height */
	unsigned short MAX_W = gfs->screen_width,
	               MAX_H = gfs->screen_height;

	for (i = 0; i < gfs->nimages; i++)
		Gif_ClipImage(gfs->images[i], 0, 0, MAX_W, MAX_H);

	/* choose background */
	if (gfs->images[0]->transparent < 0
		&& gfs->global && gfs->background < in_global_map->ncol)
		background = in_global_map->col[gfs->background].pixel;
	else
		background = TColorEmpty;
}

static void finalize_optimizer(Gif_Stream *gfs, bool del_empty)
{
	if (background == TColorEmpty)
		gfs->background = (unsigned short)gfs->images[0]->transparent;

	/* 11.Mar.2010 - remove entirely transparent frames. */
	for (unsigned i = 1; i < gfs->nimages && del_empty; i++) {
		Gif_Image *curr = gfs->images[i];
		Gif_Image *prev = gfs->images[i - 1];
		if (curr->width  == 1 && !curr->identifier &&
			curr->height == 1 && !curr->comment    &&
			curr->transparent >= 0 && curr->delay  && prev->delay && (
			curr->disposal == GD_Asis ||
			curr->disposal == GD_None ||
			curr->disposal == GD_Previous
		)) {
			Gif_UncompressImage(gfs, curr);
			if (curr->img[0][0] == curr->transparent && (
				prev->disposal  == GD_Asis ||
				prev->disposal  == GD_None
			)) {
				prev->delay += curr->delay;
				Gif_DeleteImage(curr);
				memmove(&gfs->images[i], &gfs->images[i + 1], sizeof(Gif_Image *) * (gfs->nimages - i - 1));
				gfs->nimages--, i--;
			}
		}
	}
	/* 10.Dec.1998 - prefer GIF_DISPOSAL_NONE to GIF_DISPOSAL_ASIS. This is
	  semantically "wrong" -- it's better to set the disposal explicitly than
	  rely on default behavior -- but will result in smaller GIF files, since
	  the graphic control extension can be left off in many cases. */
	for (unsigned i = 0; i < gfs->nimages; i++) {
		Gif_Image *curr = gfs->images[i];
		if (curr->disposal == GD_Asis && !curr->delay && curr->transparent < 0)
			curr->disposal = GD_None;
	}
	Gif_DeleteColormap(in_global_map);
	Gif_DeleteColormap(all_colormap);
}


/* two versions of the optimization template */
#define UINT_t unsigned short
#define _Ex_(t) t ## 16
#include "opttemplate.c"
#undef UINT_t
#undef _Ex_

#define UINT_t unsigned int
#define _Ex_(t) t ## 32
#include "opttemplate.c"
#undef UINT_t
#undef _Ex_


/* the interface function! */
void Gif_FullOptimizeFragments(Gif_Stream *gfs, int optimize_flags, int huge_stream, Gif_CompressInfo *gcinfo)
{
	if (!gfs->nimages)
		return;

	init_colormaps(gfs);

	if (!gcinfo)
		 gcinfo = Gif_NewCompressInfo();

	int opt_lvl = optimize_flags & GT_OPT_MASK;
	if (opt_lvl >= 3)
		gcinfo->flags |= GIF_WRITE_OPTIMIZE;

	if ((unsigned)all_colormap->ncol >= 0xFFFF) {
		create_new_image_data32(gfs, gcinfo, opt_lvl, !huge_stream);
	} else {
		create_new_image_data16(gfs, gcinfo, opt_lvl, !huge_stream);
	}
	finalize_optimizer(gfs, !(optimize_flags & GIF_OPT_KEEPEMPTY));
}
