/* optimize.c - Functions to optimize animated GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#include <gifsi.h>
#include "kcolor.h"

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

static inline kcolor kc_MakeUc(const Gif_Color gfc)
{
	kcolor kc = KC_Set(
		(gfc.R << 7) + (gfc.R >> 1),
		(gfc.G << 7) + (gfc.G >> 1),
		(gfc.B << 7) + (gfc.B >> 1)
	);
	return kc;
}

/* colormap_add: Ensure that each color in 'src' is represented in 'dst'.
   For each color 'i' in 'src', src->col[i].pixel == some j
   so that Gif_ColorEq(src->col[i], dst->col[j]).
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
		kchistitem* khi = kchist_add(cm_hist, kc_MakeUc(src->col[i]), 0);

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
static void increment_penalties(unsigned char *need, const int count, penalty_t *penalty, penalty_t delta)
{
	for (int i = 1; i < count; i++) {
		if (need[i] == TColorRequired)
			penalty[i] += delta;
	}
}


/*****
 * CREATE COLOR MAPPING FOR A PARTICULAR IMAGE
 *
 * sort_colormap_permutation_rgb: for canonicalizing local colormaps by
   arranging them in RGB order */

static int rgb_sort_cmp(const Gif_Color *col1, const Gif_Color *col2, void *_)
{
	int value1 = (col1->R << 16) | (col1->G << 8) | col1->B;
	int value2 = (col2->R << 16) | (col2->G << 8) | col2->B;
	return value1 - value2;
}


/* prepare_colormap_for: Create and return an array of bytes mapping from
   global pixel values to pixel values for this image. It may add colormap
   cells to 'into'; if there isn't enough room in 'into', it will return 0. It
   sets the 'transparent' field of 'gfi->optdata', but otherwise doesn't
   change or read it at all. */

/* If we get here, it failed! Return 0 and don't change global state. */

static unsigned char *prepare_colormap_for(
	Gif_Image     *gfi,
	Gif_Colormap  *gcm,
	Gif_Colormap  *complex_cm,
	unsigned char *need,
	const bool  is_global
) {
	Gif_Color *all_col = complex_cm->col , *col = gcm->col;
	int i, j, all_ncol = complex_cm->ncol, ncol = gcm->ncol;

	unsigned char *map = Gif_NewArray(unsigned char, all_ncol);
	bool j_used[256];

	/* keep track of which pixel indices in 'into' have been used; initially,
	   all unused */
	for (i = 0; i < 256; i++)
		j_used[i] = false;

	/* go over all non-transparent global pixels which MUST appear
	   (need[P] == TColorRequired) and place them in 'cm_local' */
	if (is_global) {

		/* fail if a needed pixel isn't in the global map */
		for (i = 1; i < all_ncol; i++) {
			if (need[i] != TColorRequired)
				continue;
			if (ncol <= (j = all_col[i].pixel))
				goto error;
			j_used[j] = true;
			   map[i] = j;
		}
	} else {
		/* always place colors in a local colormap */
		for (i = 1, j = ncol; i < all_ncol; i++) {
			if ((j_used[j] = need[i] == TColorRequired)) {
				if (ncol == 256)
					goto error;
				map[i] = j = ncol++;
				col[j] = all_col[i];
				col[j].pixel = i;
			}
		}
		qSortPerm(Gif_Color, col, ncol, rgb_sort_cmp, NULL);

		for (i = 0; i < ncol; i++)
			map[col[i].pixel] = i;
	}

	/* now check for transparency */
	short transparent = -1;
	if (need[TColorEmpty]) {
		/* first, look for an unused index in 'into'. Pick the lowest one: the
		lower transparent index we get, the more likely we can shave a bit off
		min_code_bits later, thus saving space */
		for (i = 0; i < ncol; i++) {
			if (!j_used[i]) {
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
	}
	gfi->transparent = transparent;
	/* If we get here, it worked! Commit state changes (the number of color
		cells in 'into') and return the map. */
	gcm->ncol = ncol;
	return map;

error:
	/* If we get here, it failed! Return 0 and don't change global state. */
	Gif_DeleteArray(map);
	return NULL;
}


/*****
 * INITIALIZATION AND FINALIZATION
 **/
static Gif_Colormap *init_colormaps(Gif_Stream *gfs, unsigned *bg_color)
{
	unsigned i, t;
	int first_transparent = -1;
	bool any_globals = false;

	/* combine colormaps */
	Gif_Colormap *gl_cmap = Gif_New(Gif_Colormap);

	if (Gif_InitColormap(gl_cmap, 1, 384))
		Gif_SetColor(gl_cmap->col[0], 255, 255, 255);

	if (!gfs->global) {
		Gif_NewColormap(gfs->global, 256);
		for (i = 0; i < 256; i++)
			Gif_SetColor(gfs->global->col[i], i, i, i);
	}

	/* Histogram so we can find colors quickly */
	kchist cm_hist;
	kchist_init(&cm_hist);

	for (i = 0; i < gfs->nimages; i++) {
		Gif_Image *gfi = gfs->images[i];
		if (gfi->local)
			colormap_add(gfi->local, gl_cmap, &cm_hist);
		else
			any_globals = true;
		if (gfi->transparent >= 0 && first_transparent < 0)
			first_transparent = i;
	}
	if (any_globals)
		colormap_add(gfs->global, gl_cmap, &cm_hist);
	kchist_cleanup(&cm_hist);

	/* try and maintain transparency's pixel value */
	if (first_transparent >= 0) {
		Gif_Image *gfi = gfs->images[first_transparent];
		Gif_Colormap *gfcm = gfi->local ?: gfs->global;
		gl_cmap->col[TColorEmpty] = gfcm->col[gfi->transparent];
	}
	/* find screen_width and screen_height, and clip all images to screen */
	Gif_CalculateScreenSize(gfs, 0);

	/* Screen width and height */
	unsigned short MAX_W = gfs->screen_width,
	               MAX_H = gfs->screen_height;

	for (i = 0; i < gfs->nimages; i++)
		Gif_ClipImage(gfs->images[i], 0, 0, MAX_W, MAX_H);

	/* choose bg_color */
	if (gfs->images[0]->transparent < 0 && gfs->background < gfs->global->ncol)
		*bg_color = gfs->global->col[gfs->background].pixel;
	else
		*bg_color = TColorEmpty;
	return gl_cmap;
}

static void finalize_optimizer(Gif_Stream *gfs, bool del_empty)
{
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
	unsigned bg_color;

	if (!gfs->nimages)
		return;

	/* Colormap containing all colors in the image. May have >256 colors */
	Gif_Colormap *complex_cm = init_colormaps(gfs, &bg_color);

	if (!gcinfo)
		 gcinfo = Gif_NewCompressInfo();

	int opt_lvl = optimize_flags & GIF_OPT_MASK;
	if (opt_lvl >= 3)
		gcinfo->flags |= GIF_WRITE_OPTIMIZE;

	if ((unsigned)complex_cm->ncol >= 0xFFFF) {
		create_new_image_data32(gfs, complex_cm, bg_color, opt_lvl, !huge_stream, gcinfo);
	} else {
		create_new_image_data16(gfs, complex_cm, bg_color, opt_lvl, !huge_stream, gcinfo);
	}
	if (bg_color == TColorEmpty)
		gfs->background = (unsigned short)gfs->images[0]->transparent;
	finalize_optimizer(gfs, !(optimize_flags & GIF_OPT_KEEPEMPTY));
	Gif_DeleteColormap(complex_cm);
}
