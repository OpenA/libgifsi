/* opttemplate.c - Functions to optimize animated GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

/* sort_permutation: sorts a given permutation 'perm' according to the
   corresponding values in 'values'. Thus, in the output, the sequence
   '[ values[perm[i]] | i <- 0..size-1 ]' will be monotonic, either up or
   (if is_down != 0) down. */

/* 9.Dec.1998 - Dumb idiot, it's time you stopped using C. The optimizer was
   broken because I switched to uint32_t's for the sorting values without
   considering the consequences; and the consequences were bad. */

static int _Ex_(cmp_sort_up)(
	const UINT_t *v1,
	const UINT_t *v2,
	penalty_t    *pval
) {
	penalty_t pe1 = pval[*v1], pe2 = pval[*v2];
	return (
		pe1 < pe2 ? -1 : pe1 == pe2 ? 0 : 1
	);
}

static int _Ex_(cmp_sort_down)(
	const UINT_t *v1,
	const UINT_t *v2,
	penalty_t    *pval
) {
	penalty_t pe1 = pval[*v1], pe2 = pval[*v2];
	return (
		pe1 > pe2 ? -1 : pe1 == pe2 ? 0 : 1
	);
}


/*****
 * MANIPULATING IMAGE AREAS
 **/
static void _Ex_(copy_data_area)(Gif_OptBounds ob, UINT_t *dst, UINT_t *src)
{
	dst += ob.top * ob.MAX_W + ob.left;
	src += ob.top * ob.MAX_W + ob.left;
	for (unsigned short y = 0; y < ob.height; y++) {
		memcpy(dst, src, sizeof(UINT_t) * ob.width);
		dst += ob.MAX_W;
		src += ob.MAX_W;
	}
}

static void _Ex_(erase_data_area)(Gif_OptBounds ob, UINT_t *dst)
{
	unsigned short x, y;
	dst += ob.top * ob.MAX_W + ob.left;
	for (y = 0; y < ob.height; y++) {
		for (x = 0; x < ob.width; x++)
			dst[x] = TColorEmpty;
		dst += ob.MAX_W;
	}
}


/*****
 * APPLY A GIF FRAME OR DISPOSAL TO AN IMAGE DESTINATION
 **/
static void _Ex_(apply_frame)(
	Gif_Colormap *gcm,
	Gif_Stream   *gfs,
	Gif_Image    *gfi,
	UINT_t       *dst,
	bool replace,
	const bool save_uncompressed)
{
	int i, x, y;
	bool was_compressed = false;
	UINT_t map[256];
	Gif_Colormap *colormap = gfi->local ?: gcm;
	Gif_OptBounds ob = get_safe_bounds(gfi, gfs->screen_width, gfs->screen_height);

	if (!gfi->img) {
		was_compressed = true;
		Gif_UncompressImage(gfs, gfi);
	}

	/* make sure transparency maps to TColorEmpty */
	for (i = 0; i < colormap->ncol; i++)
		map[i] = colormap->col[i].pixel;

	/* out-of-bounds colors map to 0, for the sake of argument */
	y = colormap->ncol ? colormap->col[0].pixel : 0;

	for (i = colormap->ncol; i < 256; i++)
		map[i] = y;

	if (gfi->transparent >= 0 && gfi->transparent < 256)
		map[gfi->transparent] = TColorEmpty;
	else
		replace = true;

	/* map the image */
	dst += ob.left + ob.top * (unsigned)ob.MAX_W;
	for (y = 0; y < ob.height; y++) {
		unsigned char *gfi_pointer = gfi->img[y];

		for (x = 0; x < ob.width; x++) {
			UINT_t new_pixel = map[gfi_pointer[x]];
			if (replace || new_pixel != TColorEmpty)
				dst[x] = new_pixel;
		}
		dst += ob.MAX_W;
	}
	if (was_compressed && !save_uncompressed)
		Gif_ReleaseUncompressedImage(gfi);
}


/*****
 * FIND THE SMALLEST BOUNDING RECTANGLE ENCLOSING ALL CHANGES
 *
 * find_difference_bounds: Find the smallest rectangular area containing all
   the changes and store it in 'bounds'. */

static void _Ex_(find_difference_bounds)(
	Gif_OptData  *bounds,
	Gif_OptBounds ob,

	UINT_t *last_data,
	UINT_t *this_data,

	const bool use_current_bounds
) {
	unsigned short lf, rt, lf_min, rt_max, tp, bt, x, y;

	/* 1.Aug.99 - use current bounds if possible, since this function is a speed
		bottleneck */
	if (use_current_bounds) {
		lf_min = ob.left;
		rt_max = ob.left + ob.width - 1;
		tp = ob.top;
		bt = ob.top + ob.height - 1;
	} else {
		lf_min = 0;
		rt_max = ob.MAX_W - 1;
		tp = 0;
		bt = ob.MAX_H - 1;
	}
	while (tp < ob.MAX_H && !memcmp(
		last_data + ob.MAX_W * tp,
		this_data + ob.MAX_W * tp,
		            ob.MAX_W * sizeof(UINT_t)))
		tp++;
	while (bt >= tp && !memcmp(
		last_data + ob.MAX_W * bt,
		this_data + ob.MAX_W * bt,
		            ob.MAX_W * sizeof(UINT_t)))
		bt--;
	lf = ob.MAX_W;
	rt = 0;

	for (y = tp; y <= bt; y++) {
		UINT_t *ld = last_data + ob.MAX_W * y;
		UINT_t *td = this_data + ob.MAX_W * y;
		for (x = lf_min; x < lf; x++)
			if (ld[x] != td[x])
				break;
		lf = x;

		for (x = rt_max; x > rt; x--)
			if (ld[x] != td[x])
				break;
		rt = x;
	}

	/* 19.Aug.1999 - handle case when there's no difference between frames */
	if (tp > bt) {
		tp = bt = ob.top;
		lf = rt = ob.left;
	}
	bounds->left   = lf;
	bounds->top    = tp;
	bounds->width  = rt + 1 - lf;
	bounds->height = bt + 1 - tp;
}


/* expand_difference_bounds: If the current image has background disposal and
   the background is transparent, we must expand the difference bounds to
   include any blanked (newly transparent) pixels that are still transparent
   in the next image. This function does that by comparing this_data and
   next_data. The new bounds are passed and stored in 'bounds'; the image's
   old bounds, which are also the maximum bounds, are passed in
   'this_bounds'. */

static bool _Ex_(expand_difference_bounds)(
	Gif_OptData  *bounds,
	Gif_OptBounds ob,

	UINT_t *this_data,
	UINT_t *next_data
) {
	unsigned short x, y,
	      l = ob.left, w = ob.width,
	      t = ob.top,  h = ob.height;
	bool ok = false;

	if (!bounds->width || !bounds->height) {
		bounds->left   = bounds->top = 0;
		bounds->width  = ob.MAX_W;
		bounds->height = ob.MAX_H;
	}

	/* 20.Nov.2013 - The image `bounds` might be larger than `this_bounds`
		because of a previous frame's background disposal. Don't accidentally
		shrink `this_bounds`. */
	if (l > bounds->left) {
		w = (l + w) - bounds->left;
		l = bounds->left;
	}
	if (t > bounds->top) {
		h = (t + h) - bounds->top;
		t = bounds->top;
	}
	if (l + w < bounds->left + bounds->width)
		w = bounds->left + bounds->width - l;
	if (t + h < bounds->top + bounds->height)
		h = bounds->top + bounds->height - t;

	for (; t < bounds->top; t++, h--) {
		UINT_t *targ = this_data + ob.MAX_W * t;
		UINT_t *next = next_data + ob.MAX_W * t;
		for (x = l; x < l + w; x++) {
			if ((ok = targ[x] != TColorEmpty && next[x] == TColorEmpty))
				break; //goto found_top;
		}
	}

//found_top:
	for (; t + h > bounds->top + bounds->height; h--) {
		UINT_t *targ = this_data + ob.MAX_W * (t + h - 1);
		UINT_t *next = next_data + ob.MAX_W * (t + h - 1);
		for (x = l; x < l + w; x++) {
			if ((ok = targ[x] != TColorEmpty && next[x] == TColorEmpty))
				break; //goto found_bottom;
		}
	}

//found_bottom:
	for (; l < bounds->left; l++, w--) {
		UINT_t *targ = this_data + l;
		UINT_t *next = next_data + l;
		for (y = t; y < t + h; y++) {
			if ((ok = targ[y * ob.MAX_W] != TColorEmpty
			       && next[y * ob.MAX_W] == TColorEmpty))
				break; //goto found_left;
		}
	}

//found_left:
	for (; l + w > bounds->left + bounds->width; w--) {
		UINT_t *targ = this_data + l + w - 1;
		UINT_t *next = next_data + l + w - 1;
		for (y = t; y < t + h; y++) {
			if ((ok = targ[y * ob.MAX_W] != TColorEmpty
			       && next[y * ob.MAX_W] == TColorEmpty))
				break; //goto found_right;
		}
	}

//found_right:
	for (y = t; y < t + h && ok; y++) {
		UINT_t *targ = this_data + y * ob.MAX_W;
		UINT_t *next = next_data + y * ob.MAX_W;
		for (x = l; x < l + w; x++) {
			if ((ok = targ[x] != TColorEmpty && next[x] == TColorEmpty))
				break;
		}
	}
	bounds->width  = w; bounds->left = l;
	bounds->height = h; bounds->top  = t;

	return ok;
}


/*****
 * DETERMINE WHICH COLORS ARE USED
 *
   get_used_colors: mark which colors are needed by a given image. Returns a
   need array so that need[j] == TColorRequired if the output colormap must
   include all_color j; TColorReplace if it should be replaced by
   transparency; and 0 if it's not in the image at all.

   If use_transparency > 0, then a pixel which was the same in the last frame
   may be replaced with transparency. If use_transparency == 2, transparency
   MUST be set. (This happens on the first image if the background should be
   transparent.) */

static void _Ex_(get_used_colors)(
	Gif_OptData *bounds,
	UINT_t      *last_data,
	UINT_t      *this_data,
	Gif_TranColor tColor,
	const int all_ncol,
	const unsigned short MAX_W,
	const unsigned short MAX_H
) {
	const unsigned short
		top    = bounds->top,
		left   = bounds->left,
		width  = bounds->width,
		height = bounds->height;

	int i, x, y;
	unsigned char *need = Gif_NewArray(unsigned char, all_ncol);

	for (i = 0; i < all_ncol; i++)
		need[i] = 0;

	/* set elements that are in the image. need == 2 means the color
		must be in the map; need == 1 means the color may be replaced by
		transparency. */
	for (y = top; y < (top + height); y++) {
		UINT_t *last = last_data + MAX_W * y + left;
		UINT_t *data = this_data + MAX_W * y + left;
		for (x = 0; x < width; x++) {
			if (data[x] != last[x])
				need[data[x]] = TColorRequired;
			else if (need[data[x]] == TColorEmpty)
				need[data[x]] = TColorReplace;
		}
	}
	if (need[TColorEmpty])
		need[TColorEmpty] = TColorRequired;

	/* check for too many colors; also force transparency if needed */
	int count[3] = { 0, 0, 0 };

	for (i = 0; i < all_ncol; i++)
		count[need[i]]++;

	/* If use_transparency is large and there's room, add transparency */
	if (tColor == TColorRequired && !need[TColorEmpty] && count[TColorRequired] < TColorLimit) {
		need[TColorEmpty] = TColorRequired;
		count[TColorRequired]++;
	}

	/* If too many "potentially transparent" pixels, force transparency */
	if (count[TColorReplace] + count[TColorRequired] > TColorLimit)
		tColor = TColorReplace;
	/* Make sure transparency is marked necessary if we use it */
	if (count[TColorReplace] > TColorEmpty && tColor && !need[TColorEmpty]) {
		need[TColorEmpty] = TColorRequired;
		count[TColorRequired]++;
	}
	/* If not using transparency, change "potentially transparent" pixels to
		"actually used" pixels */
	if (tColor == TColorEmpty) {
		for (i = 0; i < all_ncol; i++) {
			if (need[i] == TColorReplace)
				need[i] = TColorRequired;
		}
		count[TColorRequired] += count[TColorReplace];
	}
	/* If we can afford to have transparency, and we want to use it, then
		include it */
	if (count[TColorRequired] < TColorLimit && tColor && !need[TColorEmpty]) {
		need[TColorEmpty] = TColorRequired;
		count[TColorRequired]++;
	}
	bounds->required_color_count = count[TColorRequired];
	bounds->needed_colors = need;
}


/*****
 * FIND SUBIMAGES AND COLORS USED
 **/
static Gif_OptData **_Ex_(make_opt_samples)(
	Gif_Stream *gfs,
	UINT_t     *prev_data,
	UINT_t     *last_data,
	UINT_t     *this_data,
	unsigned    bg_color,
	const int   all_ncol,
	const int   opt_lvl,
	const bool  save_uncompressed
) {
	const unsigned short max_w = gfs->screen_width,
	                     max_h = gfs->screen_height;
	const unsigned screen_size = (unsigned)max_w * (unsigned)max_h;
	      unsigned i;

	Gif_Disposal last_disp = GD_None;
	Gif_OptData **opt_data = Gif_NewArray(Gif_OptData *, gfs->nimages);

	bool is_next_data_valid = false;

	UINT_t *next_data = Gif_NewArray(UINT_t, screen_size);

	/* do first image. Remember to uncompress it if necessary */
	for (i = 0; i < screen_size; i++)
		prev_data[i] = last_data[i] = this_data[i] = next_data[i] = TColorEmpty;

	/* PRECONDITION:
	   prev_data -- garbage
	   last_data -- optimized image after disposal of previous optimized frame
	   this_data -- input image after disposal of previous input frame
	   next_data -- input image after application of current input frame,
	                if next_image_valid */
	for (i = 0; i < gfs->nimages; i++) {

		Gif_Image *gfi = gfs->images[i];
		Gif_OptData *subimage = new_opt_data();

		/* set map of used colors */
		Gif_TranColor tColor = TColorEmpty;

		const Gif_Disposal disp = gfi->disposal;
		const Gif_OptBounds ob  = get_safe_bounds(gfi, max_w, max_h);

		bool has_loc_ctab = i && gfi->local;
		bool is_not_last  = i < gfs->nimages - 1;

		/* save previous data if necessary */
		if (disp == GD_Previous || (has_loc_ctab && last_disp > GD_Asis))
			memcpy(prev_data, this_data, sizeof(UINT_t) * screen_size);

		/* set this_data equal to the current image */
		if (is_next_data_valid) {
			_SWAP_PTR(UINT_t, this_data, next_data);
			is_next_data_valid = false;
		} else
			_Ex_(apply_frame)(gfs->global, gfs, gfi, this_data, false, save_uncompressed);

retry_frame:
		/* find minimum area of difference between this image and last image */
		subimage->disposal = GD_Asis;
		if (i > 0) {
			_Ex_(find_difference_bounds)(subimage, ob, last_data, this_data,
				(last_disp == GD_None || last_disp == GD_Asis));
			if (opt_lvl > 1)
				tColor = TColorReplace;
		} else {
			subimage->left   = ob.left;
			subimage->top    = ob.top;
			subimage->width  = ob.width;
			subimage->height = ob.height;
			if (bg_color == TColorEmpty)
				tColor = TColorRequired;
		}

		/* might need to expand difference border on background disposal */
		if (is_not_last && (disp == GD_Background || disp == GD_Previous)) {
			/* set up next_data */
			memcpy(next_data, (disp == GD_Previous ? prev_data : this_data), sizeof(UINT_t) * screen_size);
			if (disp == GD_Background)
				_Ex_(erase_data_area)(ob, next_data);
			_Ex_(apply_frame)(gfs->global, gfs, gfs->images[i + 1], next_data, false, save_uncompressed);
			is_next_data_valid = true;
			/* expand border as necessary */
			if (_Ex_(expand_difference_bounds)(subimage, ob, this_data, next_data))
				subimage->disposal = GD_Background;
		}
		fix_difference_bounds(subimage, max_w, max_h);

		_Ex_(get_used_colors)(subimage, last_data, this_data, tColor, all_ncol, max_w, max_h);
		/* Gifsicle's optimization strategy normally creates frames with ASIS
			or BACKGROUND disposal (not PREVIOUS disposal). However, there are
			cases when PREVIOUS disposal is strictly required, or a frame would
			require more than 256 colors. Detect this case and try to recover. */
		if (subimage->required_color_count > 256) {
			if (has_loc_ctab) {
				if (opt_data[i - 1]->disposal != last_disp && (
					last_disp == GD_Previous || last_disp == GD_Background
				)) {
					opt_data[i - 1]->disposal = last_disp;
					memcpy(last_data, prev_data, sizeof(UINT_t) * screen_size);
					goto retry_frame;
				}
			}
			printf("%d colors required in a frame (256 is max)", subimage->required_color_count);
			__assert_fail("libgifsi: error colors in source", __FILE__, __LINE__, __ASSERT_FUNCTION);
		}
		opt_data[i] = subimage;

		/* Apply optimized disposal to last_data and unoptimized disposal to
		this_data. Before 9.Dec.1998 I applied unoptimized disposal uniformly
		to both. This led to subtle bugs. After all, to determine bounds, we
		want to compare the current image (only obtainable through unoptimized
		disposal) with what WILL be left after the previous OPTIMIZED image's
		disposal. This fix is repeated in create_new_image_data */
		Gif_OptBounds sio = get_safe_bounds(subimage, max_w, max_h);

		if (subimage->disposal == GD_Background)
			_Ex_(erase_data_area)(sio, last_data);
		else
			_Ex_(copy_data_area)(sio, last_data, this_data);

		if (last_disp == GD_Background)
			_Ex_(erase_data_area)(get_safe_bounds(gfs->images[i - 1], max_w, max_h), this_data);
		else if (last_disp == GD_Previous)
			_SWAP_PTR(UINT_t, prev_data, this_data);

		last_disp = disp;
	}
	Gif_DeleteArray(next_data);
	return opt_data;
}


/*****
 * CALCULATE OUTPUT GLOBAL COLORMAP
 **/

/* make_opt_colormap: The interface function to this pass. It creates
   global colormap and sets pixel values on all_colormap appropriately.
   Specifically:

   all_colormap->col[P].pixel >= 256 ==> P is not in the global colormap.

   Otherwise, all_colormap->col[P].pixel == the J so that
   GIF_COLOREQ(&all_colormap->col[P], &gfs->global->col[J]).

   On return, the 'colormap_penalty' component of an image's Gif_OptData
   structure is <0 iff that image will need a local colormap.

   20.Aug.1999 - updated to new version that arranges the entire colormap, not
   just the stuff above 256 colors. */

static Gif_Colormap *_Ex_(make_opt_colormap)(
	Gif_Stream    *gfs,
	Gif_OptData  **opt,
	Gif_Colormap  *complex_cm,
	const unsigned bg_color
) {
	int c, cur_ncol, all_ncol = complex_cm->ncol;

	unsigned i;

	penalty_t *penalty  = Gif_NewArray(penalty_t, all_ncol);
	UINT_t    *permute  = Gif_NewArray(UINT_t   , all_ncol);
	UINT_t    *ordering = Gif_NewArray(UINT_t   , all_ncol);

	short nglobal_all = (all_ncol <= 257 ? all_ncol - 1 : 256);
	bool permutation_changed = true;

	assert(all_ncol <= 0x7FFFFFFF);

	/* set initial penalties and permutation for each color */
	for (c = 0; c < all_ncol; c++) {
		penalty[c] = 0;
		permute[c] = c + 1;
	}
	permute[all_ncol - 1] = 0;
	/* choose appropriate penalties for each image */
	for (i = 0; i < gfs->nimages; i++) {
		penalty_t cm_pen = (opt[i]->global_penalty = 1);
		for (unsigned pi = 2; pi < opt[i]->required_color_count; pi *= 2)
			cm_pen *= 3;
		opt[i]->colormap_penalty = cm_pen;
		opt[i]->active_penalty   = (all_ncol > 257 ? cm_pen : 1);
		increment_penalties(opt[i]->needed_colors, all_ncol, penalty, opt[i]->active_penalty);
	}

	/* Loop, removing one color at a time. */
	for (cur_ncol = all_ncol - 1; cur_ncol; cur_ncol--) {
		UINT_t removed;

		/* sort permutation based on penalty */
		if (permutation_changed)
			qSortPerm(UINT_t, permute, cur_ncol, _Ex_(cmp_sort_down), penalty);
		permutation_changed = false;

		/* update reverse permutation */
		removed = permute[cur_ncol - 1];
		ordering[removed] = cur_ncol - 1;

		/* decrement penalties for colors that are out of the running */
		for (i = 0; i < gfs->nimages; i++) {
			unsigned char *need = opt[i]->needed_colors;
			if (opt[i]->global_penalty > 0 && need[removed] == TColorRequired) {
				increment_penalties(need, all_ncol, penalty, -opt[i]->active_penalty);
				opt[i]->global_penalty   = 0;
				opt[i]->colormap_penalty = (cur_ncol > 256 ? -1 : 0);
				     permutation_changed = true;
			}
		}

		/* change colormap penalties if we're no longer working w/globalmap */
		if (cur_ncol == 257) {
			for (c = 0; c < all_ncol; c++)
				penalty[c] = 0;
			for (i = 0; i < gfs->nimages; i++) {
				increment_penalties(
				   opt[i]->needed_colors, all_ncol, penalty,
				  (opt[i]->active_penalty = opt[i]->global_penalty));
			}
			permutation_changed = true;
		}
	}

	/* make sure bg_color is in the global colormap */
	if (bg_color != TColorEmpty && ordering[bg_color] >= TColorLimit) {
		UINT_t other = permute[255];
		ordering[other] = ordering[bg_color];
		ordering[bg_color] = 255;
	}

	/* assign global colormap based on permutation */
	Gif_Colormap *old_gcm =  gfs->global,
	             *new_gcm = (gfs->global = Gif_NewColormap(nglobal_all, 256));

	for (c = 1; c < all_ncol; c++) {
		if (ordering[c] < 256) {
			new_gcm->col[ordering[c]] = complex_cm->col[c];
			complex_cm->col[c].pixel = ordering[c];
		} else
			complex_cm->col[c].pixel = TColorLimit;
	}

	/* set the stream's background color */
	if (bg_color != TColorEmpty)
		gfs->background = ordering[bg_color];

	/* cleanup */
	Gif_DeleteArray(penalty);
	Gif_DeleteArray(permute);
	Gif_DeleteArray(ordering);

	return old_gcm;
}


/*****
 * CREATE OUTPUT FRAME DATA
 * 
 * transp_frame_data: copy the frame data into the actual image, using
   transparency occasionally according to a heuristic described below */

static void _Ex_(transp_frame_data)(
	Gif_Stream *gfs,
	Gif_Image  *gfi, 
	UINT_t     *last_data,
	UINT_t     *this_data,
	const bool  need_compress,

	unsigned char    *map,
	Gif_OptBounds     ob,
	Gif_CompressInfo *gcinfo
) {
	short transparent = gfi->transparent;
	UINT_t *last = NULL, *cur = NULL;
	unsigned char *data, *begin_same, *t2_data = NULL, *last_for_t2;
	unsigned short x, y, nsame = 0;

	Gif_FullCompressImage(gfs, gfi, gcinfo);
	gcinfo->flags |= GIF_WRITE_SHRINK;

  /* Actually copy data to frame.

     Use transparency if possible to shrink the size of the written GIF.

     The written GIF will be small if patterns (sequences of pixel values)
     recur in the image.
     We could conceivably use transparency to produce THE OPTIMAL image,
     with the most recurring patterns of the best kinds; but this would
     be very hard (wouldn't it?). Instead, we settle for a heuristic:
     we try and create RUNS. (Since we *try* to create them, they will
     presumably recur!) A RUN is a series of adjacent pixels all with the
     same value.

     By & large, we just use the regular image's values. However, we might
     create a transparent run *not in* the regular image, if TWO OR MORE
     adjacent runs OF DIFFERENT COLORS *could* be made transparent.

     (An area can be made transparent if the corresponding area in the previous
     frame had the same colors as the area does now.)

     Why? If only one run (say of color C) could be transparent, we get no
     large immediate advantage from making it transparent (it'll be a run of
     the same length regardless). Also, we might LOSE: what if the run was
     adjacent to some more of color C, which couldn't be made transparent? If
     we use color C (instead of the transparent color), then we get a longer
     run.

     This simple heuristic does a little better than Gifwizard's (6/97)
     on some images, but does *worse than nothing at all* on others.

     However, it DOES do better than the complicated, greedy algorithm that
     preceded it; and now we pick either the transparency-optimized version or
     the normal version, whichever compresses smaller, for the best of both
     worlds. (9/98)

     On several images, making SINGLE color runs transparent wins over the
     previous heuristic, so try both at optimize level 3 or above (the cost is
     ~30%). (2/11) */

	data = begin_same = last_for_t2 = gfi->image_data;

	for (y = 0; y < ob.height; y++) {

		last = last_data + ob.MAX_W * (y + ob.top) + ob.left;
		cur  = this_data + ob.MAX_W * (y + ob.top) + ob.left;

		for (x = 0; x < ob.width; x++, data++, cur++, last++) {
			if (*cur != *last && map[*cur] != transparent) {
				if (nsame == 1 && data[-1] != transparent && need_compress) {
					if (!t2_data)
						t2_data = Gif_NewArray(unsigned char, (size_t)ob.width * (size_t)ob.height);
					memcpy(t2_data + (last_for_t2 - gfi->image_data),
						last_for_t2, begin_same - last_for_t2);
					memset(t2_data + (begin_same - gfi->image_data),
						transparent, data - begin_same);
					last_for_t2 = data;
				}
				nsame = 0;
			} else if (nsame == 0) {
				begin_same = data;
				nsame++;
			} else if (nsame == 1 && map[*cur] != data[-1]) {
				memset(begin_same, transparent, data - begin_same);
				nsame++;
			}
			*data = nsame > 1 ? transparent : map[*cur];
		}
	}

	if (t2_data)
		memcpy(t2_data + (last_for_t2 - gfi->image_data), last_for_t2, data - last_for_t2);

	/* Now, try compressed transparent version(s) and pick the better of the
	   two (or three). */
	Gif_FullCompressImage(gfs, gfi, gcinfo);
	if (t2_data) {
		Gif_SetUncompressedImage(gfi, t2_data, Gif_Free, false);
		Gif_FullCompressImage(gfs, gfi, gcinfo);
	}
	Gif_ReleaseUncompressedImage(gfi);

	gcinfo->flags &= ~GIF_WRITE_SHRINK;
}

static void _Ex_(make_out_frames)(
	Gif_OptData *opt,
	Gif_Stream  *gfs,
	Gif_Image   *gfi,
	UINT_t      *last_data,
	UINT_t      *this_data,
	const int    opt_lvl,
	const bool   not_first,
	const bool   was_compress,

	Gif_Colormap *complex_cm,
	Gif_CompressInfo *gcinfo
) {
	Gif_Disposal  disp = (gfi->disposal = opt->disposal);
	unsigned short top = (gfi->top      = opt->top     ),
	              left = (gfi->left     = opt->left    ),
	          x, width = (gfi->width    = opt->width   ),
	         y, height = (gfi->height   = opt->height  );

	Gif_OptBounds   ob = new_opt_bounds(
		left, top, width, height, gfs->screen_width, gfs->screen_height
	);

	if (not_first)
		gfi->interlace = 0;

	Gif_DeleteColormap(gfi->local);
	gfi->local = NULL;

	/* try to map pixel values into the global colormap */
	unsigned char *map = prepare_colormap_for(gfi, gfs->global, complex_cm, opt->needed_colors, true);

	if (!map) {
		/* that didn't work; add a local colormap. */
		map = prepare_colormap_for(gfi,
			(gfi->local = Gif_NewColormap(0, 256)), complex_cm, opt->needed_colors, false);
	}
	/* find the new image's colormap and then make new data */
	unsigned char *data = Gif_NewArray(unsigned char, (size_t)width * (size_t)height);

	/* First, try w/o transparency. Compare this to the result using
	   transparency and pick the better of the two. */
	for (y = 0; y < ob.height; y++) {
		UINT_t *from = this_data + ob.MAX_W * (y + ob.top) + ob.left;
		unsigned char *into = data + y * width;
		for (x = 0; x < ob.width; x++)
			*into++ = map[*from++];
	}
	Gif_SetUncompressedImage(gfi, data, Gif_Free, false);

	/* don't use transparency on first frame */
	if (opt_lvl > 1 && not_first && gfi->transparent >= 0) {
		_Ex_(transp_frame_data)(gfs, gfi, last_data, this_data, opt_lvl > 2, map, ob, gcinfo);
	}
	if (gfi->img) {
		if (was_compress || opt_lvl > 1) {
			Gif_FullCompressImage(gfs, gfi, gcinfo);
			Gif_ReleaseUncompressedImage(gfi);
		} else  /* bug fix 22.May.2001 */
			Gif_ReleaseCompressedImage(gfi);
	}
	Gif_DeleteArray(map);
	Gif_DeleteArray(opt->needed_colors);
	Gif_Delete(opt);

	/* Set up last_data and this_data. last_data must contain this_data + new
	disposal. this_data must contain this_data + old disposal. */
	switch(disp) {
	case GD_None:
	case GD_Asis:
		_Ex_(copy_data_area)(ob, last_data, this_data);
		break;
	case GD_Background:
		_Ex_(erase_data_area)(ob, last_data);
	case GD_Previous:
		break;
	default:
		__assert_fail("libgifsi: optimized frame has strange disposal", __FILE__, __LINE__, __ASSERT_FUNCTION);
	}
}


/*****
 * CREATE NEW IMAGE DATA
 *
   last == what last image ended up looking like
   this == what new image should look like

   last = apply O1 + dispose O1 + ... + apply On-1 + dispose On-1
   this = apply U1 + dispose U1 + ... + apply Un-1 + dispose Un-1 + apply Un

   invariant: apply O1 + dispose O1 + ... + apply Ok
   === apply U1 + dispose U1 + ... + apply Uk */

static void _Ex_(create_new_image_data)(
	Gif_Stream   *gfs,
	Gif_Colormap *complex_cm,
	unsigned      bg_color,
	const int     opt_lvl,
	const bool    save_uncompress,

	Gif_CompressInfo *gcinfo
) {
	/* placeholder; maintains pre-optimization
	   image size so we can apply background disposal */
	const unsigned short max_w = gfs->screen_width,
	                     max_h = gfs->screen_height;
	const unsigned screen_size = (unsigned)max_w * (unsigned)max_h;
	      unsigned i;

	UINT_t *prev_data = Gif_NewArray(UINT_t, screen_size);
	UINT_t *last_data = Gif_NewArray(UINT_t, screen_size);
	UINT_t *this_data = Gif_NewArray(UINT_t, screen_size);

	Gif_OptData **opt = _Ex_(make_opt_samples)(gfs, prev_data, last_data, this_data, bg_color, complex_cm->ncol, opt_lvl, save_uncompress);
	/* Return the old global colormap, and replace in stream */
	Gif_Colormap *gcm = _Ex_(make_opt_colormap)(gfs, opt, complex_cm, bg_color);

	/* do first image. Remember to uncompress it if necessary */
	for (i = 0; i < screen_size; i++)
		prev_data[i] = last_data[i] = this_data[i] = TColorEmpty;

	for (i = 0; i < gfs->nimages; i++) {

		Gif_Image *gfi = gfs->images[i];

		/* save actual bounds and disposal from unoptimized version so we can
		apply the disposal correctly next time through */
		const Gif_OptBounds ob  = get_safe_bounds(gfi, max_w, max_h);
		const Gif_Disposal disp = gfi->disposal;

		bool was_compressed = !gfi->img;

		/* save previous data if necessary */
		if (disp == GD_Previous)
			_Ex_(copy_data_area)(ob, prev_data, this_data);

		/* set up this_data to be equal to the current image */
		_Ex_(apply_frame)(gcm, gfs, gfi, this_data, false, false);

		/* set bounds and disposal from optdata */
		Gif_ReleaseUncompressedImage(gfi);

		_Ex_(make_out_frames)(opt[i], gfs, gfi, last_data, this_data, opt_lvl, i > 0, was_compressed, complex_cm, gcinfo);

		if (disp == GD_Background)
			_Ex_(erase_data_area)(ob, this_data);
		else if (disp == GD_Previous)
			_Ex_(copy_data_area)(ob, this_data, prev_data);
	}
	Gif_DeleteArray(prev_data);
	Gif_DeleteArray(last_data);
	Gif_DeleteArray(this_data);
	Gif_DeleteArray(opt);
	Gif_DeleteColormap(gcm);
}
