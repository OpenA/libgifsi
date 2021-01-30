/* opttemplate.c - Functions to optimize animated GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

static UINT_t *_Ex_(last_data);
static UINT_t *_Ex_(this_data);
static UINT_t *_Ex_(next_data);


/* sort_permutation: sorts a given permutation 'perm' according to the
   corresponding values in 'values'. Thus, in the output, the sequence
   '[ values[perm[i]] | i <- 0..size-1 ]' will be monotonic, either up or
   (if is_down != 0) down. */

/* 9.Dec.1998 - Dumb idiot, it's time you stopped using C. The optimizer was
   broken because I switched to uint32_t's for the sorting values without
   considering the consequences; and the consequences were bad. */

static int _Ex_(permuting_sorter_up)(const void *v1, const void *v2) {
	const UINT_t *n1 = (const UINT_t *)v1;
	const UINT_t *n2 = (const UINT_t *)v2;
	if (permuting_sort_values[*n1] < permuting_sort_values[*n2])
		return -1;
	else if (permuting_sort_values[*n1] == permuting_sort_values[*n2])
		return 0;
	else
		return 1;
}

static int _Ex_(permuting_sorter_down)(const void *v1, const void *v2) {
	const UINT_t *n1 = (const UINT_t *)v1;
	const UINT_t *n2 = (const UINT_t *)v2;
	if (permuting_sort_values[*n1] > permuting_sort_values[*n2])
		return -1;
	else if (permuting_sort_values[*n1] == permuting_sort_values[*n2])
		return 0;
	else
		return 1;
}

static UINT_t *
_Ex_(sort_permutation)(UINT_t *perm, int size, penalty_t *values, bool is_down) {
	permuting_sort_values = values;
	qsort(perm, size, sizeof(UINT_t), is_down ? _Ex_(permuting_sorter_down) : _Ex_(permuting_sorter_up));
	permuting_sort_values = NULL;
	return perm;
}


/*****
 * MANIPULATING IMAGE AREAS
 **/
static void _Ex_(copy_data_area)(UINT_t *dst, UINT_t *src, Gif_Image *area)
{
	if (!area)
		return;
	Gif_OptBounds ob = safe_bounds(area);
	dst += ob.top * screen_width + ob.left;
	src += ob.top * screen_width + ob.left;
	for (int y = 0; y < ob.height; y++) {
		memcpy(dst, src, sizeof(UINT_t) * ob.width);
		dst += screen_width;
		src += screen_width;
	}
}

static void _Ex_(copy_data_area_subimage)(UINT_t *dst, UINT_t *src, Gif_OptData *area)
{
	Gif_Image img;
	img.left   = area->left;
	img.top    = area->top;
	img.width  = area->width;
	img.height = area->height;
	_Ex_(copy_data_area)(dst, src, &img);
}

static void _Ex_(erase_data_area)(UINT_t *dst, Gif_Image *area)
{
	Gif_OptBounds ob = safe_bounds(area);
	dst += ob.top * screen_width + ob.left;
	for (int y = 0; y < ob.height; y++) {
		for (int x = 0; x < ob.width; x++)
			dst[x] = TRANSP;
		dst += screen_width;
	}
}

static void _Ex_(erase_data_area_subimage)(UINT_t *dst, Gif_OptData *area)
{
	Gif_Image img;
	img.left   = area->left;
	img.top    = area->top;
	img.width  = area->width;
	img.height = area->height;
	_Ex_(erase_data_area)(dst, &img);
}

static void _Ex_(erase_screen)(UINT_t *dst, const unsigned screen_size)
{
	for (unsigned i = 0; i < screen_size; i++)
		*dst++ = TRANSP;
}

/*****
 * APPLY A GIF FRAME OR DISPOSAL TO AN IMAGE DESTINATION
 **/
static void _Ex_(apply_frame)(UINT_t *dst, Gif_Stream* gfs, Gif_Image* gfi,
               bool replace, bool save_uncompressed)
{
	int i, x, y;
	bool was_compressed = false;
	UINT_t map[256];
	Gif_Colormap *colormap = gfi->local ? gfi->local : in_global_map;
	Gif_OptBounds ob = safe_bounds(gfi);

	if (!gfi->img) {
		was_compressed = true;
		Gif_UncompressImage(gfs, gfi);
	}

	/* make sure transparency maps to TRANSP */
	for (i = 0; i < colormap->ncol; i++)
		map[i] = colormap->col[i].pixel;

	/* out-of-bounds colors map to 0, for the sake of argument */
	y = colormap->ncol ? colormap->col[0].pixel : 0;

	for (i = colormap->ncol; i < 256; i++)
		map[i] = y;

	if (gfi->transparent >= 0 && gfi->transparent < 256)
		map[gfi->transparent] = TRANSP;
	else
		replace = true;

	/* map the image */
	dst += ob.left + ob.top * (unsigned) screen_width;
	for (y = 0; y < ob.height; y++) {
		unsigned char *gfi_pointer = gfi->img[y];

		for (x = 0; x < ob.width; x++) {
			UINT_t new_pixel = map[gfi_pointer[x]];
			if (replace || new_pixel != TRANSP)
				dst[x] = new_pixel;
		}
		dst += screen_width;
	}
	if (was_compressed && !save_uncompressed)
		Gif_ReleaseUncompressedImage(gfi);
}

static void
_Ex_(apply_frame_disposal)(UINT_t *into_data, UINT_t *from_data,
                        UINT_t *prev_data, Gif_Image *gfi)
{
	if (gfi->disposal == GD_Previous) {
		memcpy(into_data, prev_data, sizeof(UINT_t) * (screen_width * screen_height));
	} else {
		memcpy(into_data, from_data, sizeof(UINT_t) * (screen_width * screen_height));
		if (gfi->disposal == GD_Background)
			_Ex_(erase_data_area)(into_data, gfi);
	}
}


/*****
 * FIND THE SMALLEST BOUNDING RECTANGLE ENCLOSING ALL CHANGES
 *
 * find_difference_bounds: Find the smallest rectangular area containing all
   the changes and store it in 'bounds'. */

static void
_Ex_(find_difference_bounds)(Gif_OptData *bounds, Gif_Image *gfi, Gif_Image *last)
{
	int lf, rt, lf_min, rt_max, tp, bt, x, y;
	Gif_OptBounds ob;

	/* 1.Aug.99 - use current bounds if possible, since this function is a speed
		bottleneck */
	if (!last || last->disposal == GD_None || last->disposal == GD_Asis) {
		ob = safe_bounds(gfi);
		lf_min = ob.left;
		rt_max = ob.left + ob.width - 1;
		tp = ob.top;
		bt = ob.top + ob.height - 1;
	} else {
		lf_min = 0;
		rt_max = screen_width - 1;
		tp = 0;
		bt = screen_height - 1;
	}
	while (tp < screen_height && !memcmp(
		_Ex_(last_data) + screen_width * tp,
		_Ex_(this_data) + screen_width * tp,
		                  screen_width * sizeof(UINT_t)))
		tp++;
	while (bt >= tp && !memcmp(
		_Ex_(last_data) + screen_width * bt,
		_Ex_(this_data) + screen_width * bt,
		                  screen_width * sizeof(UINT_t)))
		bt--;
	lf = screen_width;
	rt = 0;

	for (y = tp; y <= bt; y++) {
		UINT_t *ld = _Ex_(last_data) + screen_width * y;
		UINT_t *td = _Ex_(this_data) + screen_width * y;
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
		tp = bt = gfi->top;
		lf = rt = gfi->left;
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

static bool
_Ex_(expand_difference_bounds)(Gif_OptData *bounds, Gif_Image *this_bounds)
{
	int x, y;
	bool expanded = false;
	Gif_OptBounds ob = safe_bounds(this_bounds);

	if (bounds->width <= 0 || bounds->height <= 0) {
		bounds->left   = bounds->top = 0;
		bounds->width  = screen_width;
		bounds->height = screen_height;
	}

	/* 20.Nov.2013 - The image `bounds` might be larger than `this_bounds`
		because of a previous frame's background disposal. Don't accidentally
		shrink `this_bounds`. */
	if (ob.left > bounds->left) {
		ob.width = (ob.left + ob.width) - bounds->left;
		ob.left  = bounds->left;
	}
	if (ob.top > bounds->top) {
		ob.height = (ob.top + ob.height) - bounds->top;
		ob.top = bounds->top;
	}
	if (ob.left + ob.width < bounds->left + bounds->width)
		ob.width = bounds->left + bounds->width - ob.left;
	if (ob.top + ob.height < bounds->top + bounds->height)
		ob.height = bounds->top + bounds->height - ob.top;

	for (; ob.top < bounds->top; ob.top++, ob.height--) {
		UINT_t *targ = _Ex_(this_data) + screen_width * ob.top;
		UINT_t *next = _Ex_(next_data) + screen_width * ob.top;
		for (x = ob.left; x < ob.left + ob.width; x++) {
			if (targ[x] != TRANSP && next[x] == TRANSP) {
				expanded = true;
				goto found_top;
			}
		}
	}

found_top:
	for (; ob.top + ob.height > bounds->top + bounds->height; ob.height--) {
		UINT_t *targ = _Ex_(this_data) + screen_width * (ob.top + ob.height - 1);
		UINT_t *next = _Ex_(next_data) + screen_width * (ob.top + ob.height - 1);
		for (x = ob.left; x < ob.left + ob.width; x++) {
			if (targ[x] != TRANSP && next[x] == TRANSP) {
				expanded = true;
				goto found_bottom;
			}
		}
	}

found_bottom:
	for (; ob.left < bounds->left; ob.left++, ob.width--) {
		UINT_t *targ = _Ex_(this_data) + ob.left;
		UINT_t *next = _Ex_(next_data) + ob.left;
		for (y = ob.top; y < ob.top + ob.height; y++) {
			if (targ[y * screen_width] != TRANSP
			 && next[y * screen_width] == TRANSP) {
				expanded = true;
				goto found_left;
			}
		}
	}

found_left:
	for (; ob.left + ob.width > bounds->left + bounds->width; ob.width--) {
		UINT_t *targ = _Ex_(this_data) + ob.left + ob.width - 1;
		UINT_t *next = _Ex_(next_data) + ob.left + ob.width - 1;
		for (y = ob.top; y < ob.top + ob.height; y++) {
			if (targ[y * screen_width] != TRANSP
			 && next[y * screen_width] == TRANSP) {
				expanded = true;
				goto found_right;
			}
		}
	}

found_right:
	if (!expanded) {
		for (y = ob.top; y < ob.top + ob.height; y++) {
		UINT_t *targ = _Ex_(this_data) + y * screen_width;
		UINT_t *next = _Ex_(next_data) + y * screen_width;
		for (x = ob.left; x < ob.left + ob.width; x++)
			if (targ[x] != TRANSP && next[x] == TRANSP) {
				expanded = true;
				break;
			}
		}
	}

	bounds->left   = ob.left;
	bounds->top    = ob.top;
	bounds->width  = ob.width;
	bounds->height = ob.height;
	return expanded;
}


/*****
 * DETERMINE WHICH COLORS ARE USED
 *
   get_used_colors: mark which colors are needed by a given image. Returns a
   need array so that need[j] == REQUIRED if the output colormap must
   include all_color j; REPLACE_TRANSP if it should be replaced by
   transparency; and 0 if it's not in the image at all.

   If use_transparency > 0, then a pixel which was the same in the last frame
   may be replaced with transparency. If use_transparency == 2, transparency
   MUST be set. (This happens on the first image if the background should be
   transparent.) */

static void _Ex_(get_used_colors)(Gif_OptData *bounds, int use_transparency)
{
	const unsigned short
		top    = bounds->top,
		left   = bounds->left,
		width  = bounds->width,
		height = bounds->height;

	const int all_ncol = all_colormap->ncol;

	int i, x, y;
	unsigned char *need = Gif_NewArray(unsigned char, all_ncol);

	for (i = 0; i < all_ncol; i++)
		need[i] = 0;

	/* set elements that are in the image. need == 2 means the color
		must be in the map; need == 1 means the color may be replaced by
		transparency. */
	for (y = top; y < (top + height); y++) {
		UINT_t *data = _Ex_(this_data) + screen_width * y + left;
		UINT_t *last = _Ex_(last_data) + screen_width * y + left;
		for (x = 0; x < width; x++) {
			if (data[x] != last[x])
				need[data[x]] = REQUIRED;
			else if (need[data[x]] == 0)
				need[data[x]] = REPLACE_TRANSP;
		}
	}
	if (need[TRANSP])
		need[TRANSP] = REQUIRED;

	/* check for too many colors; also force transparency if needed */
	int count[3];

	/* Count distinct pixels in each category */
	count[0] = count[1] = count[2] = 0;

	for (i = 0; i < all_ncol; i++)
		count[need[i]]++;

	/* If use_transparency is large and there's room, add transparency */
	if (use_transparency > 1 && !need[TRANSP] && count[REQUIRED] < 256) {
		need[TRANSP] = REQUIRED;
		count[REQUIRED]++;
	}

	/* If too many "potentially transparent" pixels, force transparency */
	if (count[REPLACE_TRANSP] + count[REQUIRED] > 256)
		use_transparency = 1;
	/* Make sure transparency is marked necessary if we use it */
	if (count[REPLACE_TRANSP] > 0 && use_transparency && !need[TRANSP]) {
		need[TRANSP] = REQUIRED;
		count[REQUIRED]++;
	}
	/* If not using transparency, change "potentially transparent" pixels to
		"actually used" pixels */
	if (!use_transparency) {
		for (i = 0; i < all_ncol; i++) {
			if (need[i] == REPLACE_TRANSP)
				need[i] = REQUIRED;
		}
		count[REQUIRED] += count[REPLACE_TRANSP];
	}
	/* If we can afford to have transparency, and we want to use it, then
		include it */
	if (count[REQUIRED] < 256 && use_transparency && !need[TRANSP]) {
		need[TRANSP] = REQUIRED;
		count[REQUIRED]++;
	}
	bounds->required_color_count = count[REQUIRED];
	bounds->needed_colors = need;
}


/*****
 * FIND SUBIMAGES AND COLORS USED
 **/
static void
_Ex_(create_subimages)(Gif_Stream *gfs, int optimize_flags, bool save_uncompressed)
{
	Gif_Image *last_gfi = NULL;

	int local_color_tables = 0;
	int next_data_valid = 0;

	unsigned screen_size = screen_width * screen_height;

	UINT_t *temp_data = Gif_NewArray(UINT_t, screen_size);

	_Ex_(last_data) = Gif_NewArray(UINT_t, screen_size);
	_Ex_(this_data) = Gif_NewArray(UINT_t, screen_size);
	_Ex_(next_data) = Gif_NewArray(UINT_t, screen_size);

	/* do first image. Remember to uncompress it if necessary */
	_Ex_(erase_screen)(_Ex_(last_data), screen_size);
	_Ex_(erase_screen)(_Ex_(this_data), screen_size);

	/* PRECONDITION:
	   temp_data -- garbage
	   last_data -- optimized image after disposal of previous optimized frame
	   this_data -- input image after disposal of previous input frame
	   next_data -- input image after application of current input frame,
	                if next_image_valid */
	for (image_index = 0; image_index < gfs->nimages; image_index++) {

		Gif_Image *gfi = gfs->images[image_index];
		Gif_OptData *subimage = new_opt_data();

		if (gfi->local)
			local_color_tables = 1;

		/* save previous data if necessary */
		if (gfi->disposal == GD_Previous || (
			local_color_tables && image_index > 0 && last_gfi->disposal > GD_Asis
		)) {
			memcpy(temp_data, _Ex_(this_data), sizeof(UINT_t) * screen_size);
		}

		/* set this_data equal to the current image */
		if (next_data_valid) {
			UINT_t *temp = _Ex_(this_data);
			_Ex_(this_data) = _Ex_(next_data);
			_Ex_(next_data) = temp;
			next_data_valid = 0;
		} else
			_Ex_(apply_frame)(_Ex_(this_data), gfs, gfi, false, save_uncompressed);

retry_frame:
		/* find minimum area of difference between this image and last image */
		subimage->disposal = GD_Asis;
		if (image_index > 0) {
			_Ex_(find_difference_bounds)(subimage, gfi, last_gfi);
		} else {
			Gif_OptBounds ob = safe_bounds(gfi);
			subimage->left   = ob.left;
			subimage->top    = ob.top;
			subimage->width  = ob.width;
			subimage->height = ob.height;
		}

		/* might need to expand difference border on background disposal */
		if(( gfi->disposal == GD_Background
		  || gfi->disposal == GD_Previous)
		  && image_index < gfs->nimages - 1
		) {
			/* set up next_data */
			Gif_Image *next_gfi = gfs->images[image_index + 1];
			_Ex_(apply_frame_disposal)(_Ex_(next_data), _Ex_(this_data), temp_data, gfi);
			_Ex_(apply_frame)(_Ex_(next_data), gfs, next_gfi, false, save_uncompressed);
			next_data_valid = 1;
			/* expand border as necessary */
			if (_Ex_(expand_difference_bounds)(subimage, gfi))
				subimage->disposal = GD_Background;
		}
		fix_difference_bounds(subimage);

		/* set map of used colors */
		int use_transparency = image_index > 0 ? (optimize_flags & GT_OPT_MASK) > 1 : background == TRANSP ? 2 : 0;

		_Ex_(get_used_colors)(subimage, use_transparency);
		/* Gifsicle's optimization strategy normally creates frames with ASIS
			or BACKGROUND disposal (not PREVIOUS disposal). However, there are
			cases when PREVIOUS disposal is strictly required, or a frame would
			require more than 256 colors. Detect this case and try to recover. */
		if (subimage->required_color_count > 256) {
			if (image_index > 0 && local_color_tables) {
				Gif_OptData *subimage = (Gif_OptData*) last_gfi->user_data;
				if(( last_gfi->disposal == GD_Previous
				  || last_gfi->disposal == GD_Background)
				  && subimage->disposal != last_gfi->disposal
				) {
					subimage->disposal = last_gfi->disposal;
					memcpy(_Ex_(last_data), temp_data, sizeof(UINT_t) * screen_size);
					goto retry_frame;
				}
			}
			fatal_error("%d colors required in a frame (256 is max)", subimage->required_color_count);
		}
		gfi->user_data = subimage;
		last_gfi = gfi;

		/* Apply optimized disposal to last_data and unoptimized disposal to
		this_data. Before 9.Dec.1998 I applied unoptimized disposal uniformly
		to both. This led to subtle bugs. After all, to determine bounds, we
		want to compare the current image (only obtainable through unoptimized
		disposal) with what WILL be left after the previous OPTIMIZED image's
		disposal. This fix is repeated in create_new_image_data */
		if (subimage->disposal == GD_Background)
			_Ex_(erase_data_area_subimage)(_Ex_(last_data), subimage);
		else
			_Ex_(copy_data_area_subimage)(_Ex_(last_data), _Ex_(this_data), subimage);

		if (last_gfi->disposal == GD_Background)
			_Ex_(erase_data_area)(_Ex_(this_data), last_gfi);
		else if (last_gfi->disposal == GD_Previous) {
			UINT_t *temp = temp_data;
			temp_data = _Ex_(this_data);
			_Ex_(this_data) = temp;
		}
	}
	Gif_DeleteArray(_Ex_(next_data));
	Gif_DeleteArray(temp_data);
}


/*****
 * CALCULATE OUTPUT GLOBAL COLORMAP
 **/

/* create_out_global_map: The interface function to this pass. It creates
   out_global_map and sets pixel values on all_colormap appropriately.
   Specifically:

   all_colormap->col[P].pixel >= 256 ==> P is not in the global colormap.

   Otherwise, all_colormap->col[P].pixel == the J so that
   GIF_COLOREQ(&all_colormap->col[P], &out_global_map->col[J]).

   On return, the 'colormap_penalty' component of an image's Gif_OptData
   structure is <0 iff that image will need a local colormap.

   20.Aug.1999 - updated to new version that arranges the entire colormap, not
   just the stuff above 256 colors. */

static void _Ex_(create_out_global_map)(Gif_Stream *gfs)
{
	int c, i, cur_ncol, all_ncol = all_colormap->ncol;

	penalty_t *penalty  = Gif_NewArray(penalty_t, all_ncol);
	UINT_t    *permute  = Gif_NewArray(UINT_t   , all_ncol);
	UINT_t    *ordering = Gif_NewArray(UINT_t   , all_ncol);

	short nglobal_all = (all_ncol <= 257 ? all_ncol - 1 : 256);
	bool permutation_changed = true;

	assert(all_ncol <= 0x7FFFFFFF);

	/* initial permutation is null */
	for (c = 0; c < all_ncol - 1; c++)
		permute[c] = c + 1;

	/* choose appropriate penalties for each image */
	for (i = 0; i < gfs->nimages; i++) {
		Gif_OptData *opt = (Gif_OptData *)gfs->images[i]->user_data;
		opt->global_penalty = opt->colormap_penalty = 1;
		for (UINT_t pi = 2; pi < opt->required_color_count; pi *= 2)
			opt->colormap_penalty *= 3;
		opt->active_penalty = (all_ncol > 257 ? opt->colormap_penalty : opt->global_penalty);
	}

	/* set initial penalties for each color */
	for (c = 1; c < all_ncol; c++)
		penalty[c] = 0;

	for (i = 0; i < gfs->nimages; i++) {
		Gif_OptData *opt = (Gif_OptData *)gfs->images[i]->user_data;
		increment_penalties(opt, penalty, opt->active_penalty);
	}

	/* Loop, removing one color at a time. */
	for (cur_ncol = all_ncol - 1; cur_ncol; cur_ncol--) {
		UINT_t removed;

		/* sort permutation based on penalty */
		if (permutation_changed)
			_Ex_(sort_permutation)(permute, cur_ncol, penalty, true);
		permutation_changed = false;

		/* update reverse permutation */
		removed = permute[cur_ncol - 1];
		ordering[removed] = cur_ncol - 1;

		/* decrement penalties for colors that are out of the running */
		for (i = 0; i < gfs->nimages; i++) {
			Gif_OptData *opt = (Gif_OptData *)gfs->images[i]->user_data;
			unsigned char *need = opt->needed_colors;
			if (opt->global_penalty > 0 && need[removed] == REQUIRED) {
				increment_penalties(opt, penalty, -opt->active_penalty);
				opt->global_penalty   = 0;
				opt->colormap_penalty = (cur_ncol > 256 ? -1 : 0);
				permutation_changed   = true;
			}
		}

		/* change colormap penalties if we're no longer working w/globalmap */
		if (cur_ncol == 257) {
			for (c = 0; c < all_ncol; c++)
				penalty[c] = 0;
			for (i = 0; i < gfs->nimages; i++) {
				Gif_OptData *opt = (Gif_OptData *)gfs->images[i]->user_data;
				opt->active_penalty = opt->global_penalty;
				increment_penalties(opt, penalty, opt->global_penalty);
			}
			permutation_changed = true;
		}
	}

	/* make sure background is in the global colormap */
	if (background != TRANSP && ordering[background] >= 256) {
		UINT_t other = permute[255];
		ordering[other] = ordering[background];
		ordering[background] = 255;
	}

	/* assign out_global_map based on permutation */
	out_global_map = Gif_NewFullColormap(nglobal_all, 256);

	for (c = 1; c < all_ncol; c++) {
		if (ordering[c] < 256) {
			out_global_map->col[ordering[c]] = all_colormap->col[c];
			all_colormap->col[c].pixel = ordering[c];
		} else
			all_colormap->col[c].pixel = NOT_IN_OUT_GLOBAL;
	}

	/* set the stream's background color */
	if (background != TRANSP)
		gfs->background = ordering[background];

	/* cleanup */
	Gif_DeleteArray(penalty);
	Gif_DeleteArray(permute);
	Gif_DeleteArray(ordering);
}


/*****
 * CREATE OUTPUT FRAME DATA
 * 
 * simple_frame_data: just copy the data from the image into the frame data.
   No funkiness, no transparency, nothing */

static void _Ex_(simple_frame_data)(Gif_Image *gfi, unsigned char *map)
{
	Gif_OptBounds ob = safe_bounds(gfi);
	unsigned scan_width = gfi->width;

	for (int y = 0; y < ob.height; y++) {
		UINT_t *from = _Ex_(this_data) + screen_width * (y + ob.top) + ob.left;
		unsigned char *into = gfi->image_data + y * scan_width;
		for (int x = 0; x < ob.width; x++)
			*into++ = map[*from++];
	}
}

/* transp_frame_data: copy the frame data into the actual image, using
   transparency occasionally according to a heuristic described below */

static void
_Ex_(transp_frame_data)(Gif_Stream *gfs, Gif_Image *gfi, unsigned char *map,
                     int optimize_flags, Gif_CompressInfo *gcinfo)
{
	Gif_OptBounds ob = safe_bounds(gfi);
	int x, y, transparent = gfi->transparent;
	UINT_t *last = NULL, *cur = NULL;
	unsigned char *data, *begin_same, *t2_data = NULL, *last_for_t2;
	int nsame = 0;

  /* First, try w/o transparency. Compare this to the result using
     transparency and pick the better of the two. */
	_Ex_(simple_frame_data)(gfi, map);
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

		last = _Ex_(last_data) + screen_width * (y + ob.top) + ob.left;
		cur  = _Ex_(this_data) + screen_width * (y + ob.top) + ob.left;

		for (x = 0; x < ob.width; x++, data++, cur++, last++) {
			if (*cur != *last && map[*cur] != transparent) {
				if (nsame == 1 && data[-1] != transparent && (optimize_flags & GT_OPT_MASK) > 2) {
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
		Gif_SetUncompressedImage(gfi, t2_data, Gif_Free, 0);
		Gif_FullCompressImage(gfs, gfi, gcinfo);
	}
	Gif_ReleaseUncompressedImage(gfi);

	gcinfo->flags &= ~GIF_WRITE_SHRINK;
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

static void _Ex_(create_new_image_data)(Gif_Stream *gfs, Gif_CompressInfo *gcinfo, int optimize_flags)
{
	Gif_Image cur_unopt_gfi;
	/* placeholder; maintains pre-optimization
	   image size so we can apply background disposal */
	unsigned screen_size = screen_width * screen_height;

	UINT_t *temp_data = Gif_NewArray(UINT_t, screen_size);

	if ((optimize_flags & GT_OPT_MASK) >= 3)
		gcinfo->flags |= GIF_WRITE_OPTIMIZE;

	gfs->global = out_global_map;

	/* do first image. Remember to uncompress it if necessary */
	_Ex_(erase_screen)(_Ex_(last_data), screen_size);
	_Ex_(erase_screen)(_Ex_(this_data), screen_size);

	for (image_index = 0; image_index < gfs->nimages; image_index++) {

		Gif_Image *cur_gfi = gfs->images[image_index];
		Gif_OptData *opt = (Gif_OptData *)cur_gfi->user_data;

		bool was_compressed = cur_gfi->img == NULL;

		/* save previous data if necessary */
		if (cur_gfi->disposal == GD_Previous) {
			_Ex_(copy_data_area)(temp_data, _Ex_(this_data), cur_gfi);
		}

		/* set up this_data to be equal to the current image */
		_Ex_(apply_frame)(_Ex_(this_data), gfs, cur_gfi, false, false);

		/* save actual bounds and disposal from unoptimized version so we can
		apply the disposal correctly next time through */
		cur_unopt_gfi = *cur_gfi;

		/* set bounds and disposal from optdata */
		Gif_ReleaseUncompressedImage(cur_gfi);

		cur_gfi->left     = opt->left;
		cur_gfi->top      = opt->top;
		cur_gfi->width    = opt->width;
		cur_gfi->height   = opt->height;
		cur_gfi->disposal = opt->disposal;

		if (image_index > 0)
			cur_gfi->interlace = 0;

		/* find the new image's colormap and then make new data */
		unsigned char *map = prepare_colormap(cur_gfi, opt->needed_colors);
		unsigned char *data = Gif_NewArray(unsigned char, (size_t)cur_gfi->width * (size_t)cur_gfi->height);

		Gif_SetUncompressedImage(cur_gfi, data, Gif_Free, false);

		/* don't use transparency on first frame */
		if ((optimize_flags & GT_OPT_MASK) > 1 && image_index > 0
			&& cur_gfi->transparent >= 0)
			_Ex_(transp_frame_data)(gfs, cur_gfi, map, optimize_flags, gcinfo);
		else
			_Ex_(simple_frame_data)(cur_gfi, map);

		if (cur_gfi->img) {
			if (was_compressed || (optimize_flags & GT_OPT_MASK) > 1) {
				Gif_FullCompressImage(gfs, cur_gfi, gcinfo);
				Gif_ReleaseUncompressedImage(cur_gfi);
			} else  /* bug fix 22.May.2001 */
				Gif_ReleaseCompressedImage(cur_gfi);
		}
		Gif_DeleteArray(map);

		delete_opt_data(opt);
		cur_gfi->user_data = 0;

		/* Set up last_data and this_data. last_data must contain this_data + new
		disposal. this_data must contain this_data + old disposal. */
		if (cur_gfi->disposal == GD_None || cur_gfi->disposal == GD_Asis)
			_Ex_(copy_data_area)(_Ex_(last_data), _Ex_(this_data), cur_gfi);
		else if (cur_gfi->disposal == GD_Background)
			_Ex_(erase_data_area)(_Ex_(last_data), cur_gfi);
		else if (cur_gfi->disposal != GD_Previous)
			assert(0 && "optimized frame has strange disposal");

		if (cur_unopt_gfi.disposal == GD_Background)
			_Ex_(erase_data_area)(_Ex_(this_data), &cur_unopt_gfi);
		else if (cur_unopt_gfi.disposal == GD_Previous)
			_Ex_(copy_data_area)(_Ex_(this_data), temp_data, &cur_unopt_gfi);
	}
	Gif_DeleteArray(temp_data);
}


/*****
 * INITIALIZATION AND FINALIZATION
 **/
static void _Ex_(finalize_optimizer_data)(void) {
	Gif_DeleteArray(_Ex_(last_data));
	Gif_DeleteArray(_Ex_(this_data));
}
