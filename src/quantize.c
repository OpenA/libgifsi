/* quantize.c - Histograms and quantization for gifsicle.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#include "kmatrix.h"
#include "kcolor.h"

#include <math.h>

typedef void(*_dith_work_fn)( Gif_Image *, unsigned char *, Gif_Colormap *,
                              kd3_tree  *, unsigned int  *, Gif_ColorTransform *);

static inline void unmark_pixels(Gif_Colormap *gfcm)
{
	for (int i = 0; i < gfcm->ncol; i++)
		gfcm->col[i].haspixel = 0;
}

static void kchist_make(kchist *kch, Gif_Stream *gfs, unsigned *ntransp_store, Gif_ColorTransform *cot)
{
	int c;
	unsigned short x, y;
	unsigned i, gcount[256], lcount[256];
	unsigned transp_count = 0, bg_count = 0;
	kchist_init(kch);

	for (c = 0; c < 256; c++)
		gcount[c] = 0;

	/* Count pixels */
	for (i = 0; i < gfs->nimages; i++)
	{
		Gif_Image    *gfi = gfs->images[i];
		bool was_compress = gfi->img   == NULL,
		         is_local = gfi->local != NULL;
		short      transp = gfi->transparent;
		unsigned   *count = is_local ? lcount : gcount,
		  prev_transp_cnt = 0;

		if (is_local) {
			for (c = 0; c < 256; c++)
				count[c] = 0;
		}
		if (transp >= 0)
			prev_transp_cnt = count[transp];

		/* unoptimize the image if necessary */
		if (was_compress)
			Gif_UncompressImage(gfs, gfi);

		/* sweep over the image data, counting pixels */
		for (y = 0; y < gfi->height; y++) {
			for (x = 0; x < gfi->width; x++)
				count[ gfi->img[y][x] ] += 1;
		}
		/* add counted colors to global histogram (local only) */
		if (is_local) {
			Gif_Colormap *cm = gfi->local;
			for (c = 0; c < cm->ncol; c++) {
				if (count[c] && c != transp)
					kchist_add(kch, kc_Make8g(cm->col[c], cot), count[c]);
			}
		}
		if (transp >= 0 && count[transp] != prev_transp_cnt) {
			transp_count += count[transp] - prev_transp_cnt;
			count[transp] = prev_transp_cnt;
		}
		/* if this image has background disposal, count its size towards the
		   background's pixel count */
		if (gfi->disposal == GD_Background)
			bg_count += (unsigned)gfi->width * (unsigned)gfi->height;

		/* throw out compressed image if necessary */
		if (was_compress)
			Gif_ReleaseUncompressedImage(gfi);
	}
	Gif_Colormap *gcm = gfs->global;
	    bool bg_alpha = gfs->images[0]->transparent >= 0;

	if (!bg_alpha && gcm && gfs->background < gcm->ncol)
		gcount[gfs->background] += bg_count;
	else
		transp_count += bg_count;

	if (gcm) {
		for (c = 0; c < gcm->ncol; c++) {
			if (gcount[c])
				kchist_add(kch, kc_Make8g(gcm->col[c], cot), gcount[c]);
		}
	}
	/* now, make the linear histogram from the hashed histogram */
	kchist_compress(kch);
	*ntransp_store = transp_count;
}

static int kchistitem_sort_cmp(const kchistitem *_a, const kchistitem *_b, int *_c)
{
	const int i = *_c;
	return _a->ka.a[i] - _b->ka.a[i];
}

static int popularity_kchistitem_compare(const void *va, const void *vb)
{
	const kchistitem *a = (const kchistitem *)va;
	const kchistitem *b = (const kchistitem *)vb;
	return a->count > b->count ? -1 : a->count != b->count;
}

static int popularity_sort_compare(const void *va, const void *vb)
{
	const Gif_Color *a = (const Gif_Color *)va;
	const Gif_Color *b = (const Gif_Color *)vb;
	return a->pixel > b->pixel ? -1 : a->pixel != b->pixel;
}


/* COLORMAP FUNCTIONS return a palette (a vector of Gif_Colors). The
   pixel fields are undefined; the haspixel fields are all 0. */

static void colormap_median_cut(kchist *kch, Gif_Colormap *gfcm, Gif_ColorTransform *cot)
{
	const int adsize = gfcm->ncol;
	Gif_Color *adapt = gfcm->col;
	int nadapt, i, j;

  /* This code was written with reference to ppmquant by Jef Poskanzer,
     part of the pbmplus package. */

	unsigned Aslot_pixel[adsize];
	     int Aslot_size [adsize],
	         Aslot_first[adsize];

	/* 1. set up the first slot, containing all pixels. */
	Aslot_first[0] = 0;
	Aslot_size [0] = kch->n;
	Aslot_pixel[0] = 0;

	for (i = 0; i < kch->n; i++)
		Aslot_pixel[0] += kch->h[i].count;

	/* 2. split slots until we have enough. */
	for (nadapt = 1; nadapt < adsize; nadapt++)
	{
		kcolor minc, maxc;
		kchistitem *slice;

		/* 2.1. pick the slot to split. */
		unsigned split_pixel = 0;
		     int split_size  = 0,
		         split_first = 0;

		int kk = -1;
		for (i = 0; i < nadapt; i++) {
			if (Aslot_size[i] >= 2 && Aslot_pixel[i] > split_pixel) {
				split_first = Aslot_first[i];
				split_size  = Aslot_size [i];
				split_pixel = Aslot_pixel[i];
				kk = i;
			}
		}
		if (kk == -1)
			break;
		slice = &kch->h[split_first];

		/* 2.2. find its extent. */
		minc = maxc = slice->ka.k;
		for (i = 1; i < split_size; i++) {
			short _R = slice[i].ka.a[0],
			      _G = slice[i].ka.a[1],
			      _B = slice[i].ka.a[2];

			minc.a[0] = _MIN(minc.a[0], _R); maxc.a[0] = _MAX(maxc.a[0], _R);
			minc.a[1] = _MIN(minc.a[1], _G); maxc.a[1] = _MAX(maxc.a[1], _G);
			minc.a[2] = _MIN(minc.a[2], _B); maxc.a[2] = _MAX(maxc.a[2], _B);
		}

		/* 2.3. decide how to split it. use the luminance method. also sort
		   the colors. */
		{
			double R_diff = 0.299 * (maxc.a[0] - minc.a[0]);
			double G_diff = 0.587 * (maxc.a[1] - minc.a[1]);
			double B_diff = 0.114 * (maxc.a[2] - minc.a[2]);
			int    C_num  = (
				R_diff >= G_diff &&
				R_diff >= B_diff ? 0 :
				G_diff >= B_diff ? 1 : 2
			);
			qSortPerm(kchistitem, slice, split_size, kchistitem_sort_cmp, &C_num);
		}

		/* 2.4. decide where to split the slot and split it there. */
		{
			unsigned half_pixels = split_pixel / 2;
			unsigned pixel_accum = slice[0].count;
			unsigned diff1, diff2;
			for (i = 1; i < split_size - 1 && pixel_accum < half_pixels; i++)
				pixel_accum += slice[i].count;

		/* We know the area before the split has more pixels than the
		   area after, possibly by a large margin (bad news). If it
		   would shrink the margin, change the split. */
			diff1 = 2 * pixel_accum - split_pixel;
			diff2 = split_pixel - 2 * (pixel_accum - slice[i - 1].count);
			if (diff2 < diff1 && i > 1) {
				pixel_accum -= slice[--i].count;
			}
			Aslot_first[nadapt] = split_first + i;
			Aslot_size [nadapt] = split_size  - i;
			Aslot_pixel[nadapt] = split_pixel - pixel_accum;
			Aslot_size [kk]     = i;
			Aslot_pixel[kk]     = pixel_accum;
		}
	}

	/* 3. make the new palette by choosing one color from each slot. */
	for (i = 0; i < nadapt; i++) {
		double px[3] = { 0, 0, 0 };
		kchistitem *slice = &kch->h[Aslot_first[i]];
		kcolor kc;
		for (j = 0; j < Aslot_size[i]; j++) {
			px[0] += slice[j].ka.a[0] * (double)slice[j].count;
			px[1] += slice[j].ka.a[1] * (double)slice[j].count;
			px[2] += slice[j].ka.a[2] * (double)slice[j].count;
		}
		kc.a[0] = (int)(px[0] / Aslot_pixel[i]);
		kc.a[1] = (int)(px[1] / Aslot_pixel[i]);
		kc.a[2] = (int)(px[2] / Aslot_pixel[i]);
		adapt[i] = kc_MColR8g(kc, cot);
	}
}

void kcdiversity_init(kcdiversity *div, kchist *kch, int dodither)
{
	int i;
	div->kch = kch;
	qsort(kch->h, kch->n, sizeof(kchistitem), popularity_kchistitem_compare);
	div->closest = Gif_NewArray(int, kch->n);
	div->min_dist = Gif_NewArray(unsigned, kch->n);
	for (i = 0; i != kch->n; ++i)
		div->min_dist[i] = UINT32_MAX;
	if (dodither) {
		div->min_dither_dist = Gif_NewArray(unsigned, kch->n);
		for (i = 0; i < kch->n; i++)
			div->min_dither_dist[i] = UINT32_MAX;
	} else
		div->min_dither_dist = NULL;
	div->chosen = Gif_NewArray(int, kch->n);
	div->nchosen = 0;
}

void kcdiversity_cleanup(kcdiversity *div)
{
	Gif_DeleteArray(div->closest);
	Gif_DeleteArray(div->min_dist);
	Gif_DeleteArray(div->min_dither_dist);
	Gif_DeleteArray(div->chosen);
}

int kcdiversity_find_popular(kcdiversity *div)
{
	int i = 0, n = div->kch->n;
	while (i < n && div->min_dist[i] == 0)
		i++; /* spin */;
	return i;
}

int kcdiversity_find_diverse(kcdiversity *div, double ditherweight)
{
	int i, n = div->kch->n, chosen = kcdiversity_find_popular(div);
	if (chosen == n)
		/* skip */;
	else if (!ditherweight || !div->min_dither_dist) {
		for (i = chosen + 1; i < n; i++)
			if (div->min_dist[i] > div->min_dist[chosen])
				chosen = i;
	} else {
		double max_dist = div->min_dist[chosen] + ditherweight * div->min_dither_dist[chosen];
		for (i = chosen + 1; i < n; i++) {
			if (div->min_dist[i] != 0) {
				double dist = div->min_dist[i] + ditherweight * div->min_dither_dist[i];
				if (dist > max_dist) {
					chosen = i;
					max_dist = dist;
				}
			}
		}
	}
	return chosen;
}

int kcdiversity_choose(kcdiversity *div, int chosen, int dodither)
{
	int i, j, k, n = div->kch->n;
	kchistitem *hist = div->kch->h;

	div->min_dist[chosen] = 0;
	if (div->min_dither_dist)
		div->min_dither_dist[chosen] = 0;
	div->closest[chosen] = chosen;

	/* adjust the min_dist array */
	for (i = 0; i < n; i++) {
		if (div->min_dist[i]) {
			unsigned dist = kc_distance(&hist[i].ka.k, &hist[chosen].ka.k);
			if (dist < div->min_dist[i]) {
				div->min_dist[i] = dist;
				div->closest[i] = chosen;
			}
		}
	}
	/* also account for dither distances */
	if (dodither && div->min_dither_dist) {
		for (i = 0; i != div->nchosen; i++) {
			kcolor x = hist[chosen].ka.k, *y = &hist[div->chosen[i]].ka.k;
			/* penalize combinations with large luminance difference */
			double dL = abs(kc_luminance(&x) - kc_luminance(y));
			dL = (dL > 8192 ? dL * 4 / 32767. : 1);
			/* create combination */
			x.a[0] = (x.a[0] + y->a[0]) >> 1;
			x.a[1] = (x.a[1] + y->a[1]) >> 1;
			x.a[2] = (x.a[2] + y->a[2]) >> 1;
			/* track closeness of combination to other colors */
			for (j = 0; j < n; j++) {
				if (div->min_dist[j]) {
					double dist = kc_distance(&hist[j].ka.k, &x) * dL;
					if (dist < div->min_dither_dist[j])
						div->min_dither_dist[j] = (unsigned)dist;
				}
			}
		}
	}
	div->chosen[div->nchosen++] = chosen;
	return chosen;
}

static void colormap_diversity_do_blend(kcdiversity *div)
{
	kchistitem *hist =  div->kch->h;
	int i, j, chosenmap[div->kch->n];
	scale_color di[div->nchosen];
	for (i = 0; i < div->nchosen; i++) {
		di[i].a[0] = di[i].a[1] = di[i].a[2] = di[i].a[3] = 0;
		chosenmap[div->chosen[i]] = i;
	}
	for (i = 0; i < div->kch->n; i++) {
		double count = hist[i].count;
		if (div->closest[i] == i)
			count *= 3;
		j = chosenmap[div->closest[i]];
		di[j].a[0] += hist[i].ka.a[0] * count;
		di[j].a[1] += hist[i].ka.a[1] * count;
		di[j].a[2] += hist[i].ka.a[2] * count;
		di[j].a[3] += count;
	}
	for (i = 0; i < div->nchosen; i++) {
		int match = div->chosen[i];
		if (di[i].a[3] >= 5 * hist[match].count) {
			hist[match].ka.a[0] = (int)(di[i].a[0] / di[i].a[3]);
			hist[match].ka.a[1] = (int)(di[i].a[1] / di[i].a[3]);
			hist[match].ka.a[2] = (int)(di[i].a[2] / di[i].a[3]);
		}
	}
}

static void colormap_diversity(kchist *kch, Gif_Colormap *gfcm, Gif_ColorTransform *cot, bool do_blend)
{
	kcdiversity div;
	bool do_dither = (bool)cot->dither_plan;
	int nadapt = 0;
	int chosen;

  /* This code was uses XV's modified diversity algorithm, and was written
     with reference to XV's implementation of that algorithm by John Bradley
     <bradley@cis.upenn.edu> and Tom Lane <Tom.Lane@g.gp.cs.cmu.edu>. */

	/* 1. initialize min_dist and sort the colors in order of popularity. */
	kcdiversity_init(&div, kch, do_dither);

	/* 2. choose colors one at a time */
	for (nadapt = 0; nadapt < gfcm->ncol; nadapt++) {
		/* 2.1. choose the color to be added */
		if (nadapt == 0 || (nadapt >= 10 && nadapt % 2 == 0))
			/* 2.1a. want most popular unchosen color */
			chosen = kcdiversity_find_popular(&div);
		else if (!do_dither)
			/* 2.1b. choose based on diversity from unchosen colors */
			chosen = kcdiversity_find_diverse(&div, 0);
		else {
			/* 2.1c. choose based on diversity from unchosen colors, but allow
			   dithered combinations to stand in for colors, particularly early
			   on in the color finding process */
			/* Weight assigned to dithered combinations drops as we proceed. */
			double ditherweight = (
#if HAVE_POW
			0.05 + pow(0.25, 1 + (nadapt - 1) / 3.));
#else
			nadapt < 4 ? 0.25 : 0.125);
#endif
			chosen = kcdiversity_find_diverse(&div, ditherweight);
		}
		kcdiversity_choose(&div, chosen, nadapt > 0 && nadapt < 64 && do_dither);
	}
	/* 3. make the new palette by choosing one color from each slot. */
	if (do_blend)
		colormap_diversity_do_blend(&div);

	for (nadapt = 0; nadapt < div.nchosen; nadapt++)
		gfcm->col[nadapt] = kc_MColR8g(kch->h[div.chosen[nadapt]].ka.k, cot);
	gfcm->ncol = nadapt;

	kcdiversity_cleanup(&div);
}

Gif_Colormap *Gif_NewDiverseColormap(Gif_Stream *gfs, Gif_CDiversity alg, unsigned *ncol, Gif_ColorTransform *cot)
{
	Gif_Colormap *gfcm;
	unsigned ntransp, adapt_size = *ncol;
	kchist kch;

	/* set up the histogram */
	kchist_make(&kch, gfs, &ntransp, cot);

	if (adapt_size > kch.n)
		adapt_size = kch.n;
  /* 0. remove any transparent color from consideration; reduce adaptive
     palette size to accommodate transparency if it looks like that'll be
     necessary */
  /* It will be necessary to accommodate transparency if (1) there is
     transparency in the image; (2) the adaptive palette isn't trivial; and
     (3) there are a small number of colors in the image (arbitrary constant:
     <= 265), so it's likely that most images will use most of the slots, so
     it's likely there won't be unused slots. */
	if (adapt_size > 2 && adapt_size < kch.n && kch.n <= 265 && ntransp > 0)
		adapt_size--;

	*ncol = kch.n;
	Gif_NewColormap(gfcm, adapt_size);

	if (alg == CD_MedianCut) {
		colormap_median_cut(&kch, gfcm, cot);
	} else {
		colormap_diversity(&kch, gfcm, cot, (
			alg == CD_Blend && adapt_size >= 4));
	}
	kchist_cleanup(&kch);
	return gfcm;
}

void colormap_image_posterize(
	Gif_Image     *gfi,
	unsigned char *new_data,
	Gif_Colormap  *old_cm,
	kd3_tree      *kd3,
	unsigned      *histogram,

	Gif_ColorTransform *cot
) {
	unsigned char **pixmap = gfi->img;
	  signed short  transp = gfi->transparent;

	unsigned short x, y;
	unsigned int   i;

	Gif_Color *colors = old_cm->col;

	/* find closest colors in new colormap */
	for (i = 0; i < old_cm->ncol; i++) {
		colors[i].pixel = kd3_closest8g(kd3, colors[i], cot);
		colors[i].haspixel = 1;
	}

	/* map image */
	for (i = y = 0; y < gfi->height; y++) {
		for (x = 0; x < gfi->width;  x++, i++) {
			unsigned char pix = pixmap[y][x];
			unsigned int  col = colors[pix].pixel;
			if (pix != transp) {
				histogram[col]++;
				new_data[i] = col;
			}
		}
	}
}

#define DITHER_SCALE    1024
#define DITHER_SHIFT    10
#define DITHER_SCALE_M1 (DITHER_SCALE - 1)
#define DITHER_ITEM2ERR (1 << (DITHER_SHIFT - 7))
#define N_RANDOM_VALUES 512

void colormap_image_floyd_steinberg(
	Gif_Image     *gim,
	unsigned char *new_data,
	Gif_Colormap  *old_cm,
	kd3_tree      *kd3,
	unsigned      *histogram,

	Gif_ColorTransform *cot
) {
  /* This code was written with reference to ppmquant by Jef Poskanzer, part
     of the pbmplus package. */
	const unsigned short width  = gim->width, left = gim->left;
	const unsigned short height = gim->height, top = gim->top;
	const   signed short transp = gim->transparent;
	const unsigned int   ersize = (unsigned)width + 2;

	unsigned short x, y;
	unsigned int   i, j;
	unsigned char d0, d2;

	char direct = 1;
	bool   _0x_ = 0,
	       _1x_ = 1;

	Gif_Color *colors = old_cm->col;

	/* Initialize distances */
	for (i = 0; i < old_cm->ncol; i++) {
		colors[i].pixel = kd3_closest8g(kd3, colors[i], cot);
		colors[i].haspixel = 1;
	}

  /* Initialize Floyd-Steinberg error vectors to small random values, so we
     don't get artifacts on the top row */
	wkcolor err[2][ersize];

  /* Use the same random values on each call in an attempt to minimize
    "jumping dithering" effects on animations */
	const short *randv = (const short *)cot->dpMatrix;
	d0 = 2, d2 = 0; /* used for error diffusion */

	/* err0 initialize */
	for (i = 0; i < ersize; i++) {
		 j = (i + left) * 3;
		err[0][i].a[0] = randv[(j + 0) % N_RANDOM_VALUES];
		err[0][i].a[1] = randv[(j + 1) % N_RANDOM_VALUES];
		err[0][i].a[2] = randv[(j + 2) % N_RANDOM_VALUES];
	}
	/* err1 initialized below */

	kd3_build_xradius(kd3);

	/* Do the image! */
	for (j = x = y = 0; y < height; y++) {

		/* clear target err */
		for (i = 0; i < ersize; i++) {
			err[_1x_][i].a[0] = 0;
			err[_1x_][i].a[1] = 0;
			err[_1x_][i].a[2] = 0;
		}
		/* Do a single row */
		for (; x >= 0 && x < width; x += direct, j += direct)
		{
			unsigned char pix = gim->img[y][x];

			/* the transparent color never gets adjusted */
			if (pix == transp)
				continue;

			/* find desired new color */
			unsigned opx = old_cm->col[pix].pixel;
			kcolor use = KC_Set8g(cot->GammaTab,
				old_cm->col[pix].R,
				old_cm->col[pix].G,
				old_cm->col[pix].B
			);
			if (kd3->transform)
				kd3->transform(&use);
			/* use Floyd-Steinberg errors to adjust */
			for(int k = 0; k < 3; k++) {
				int v = (int)use.a[k] + (err[_0x_][x + 1].a[k] & ~(DITHER_ITEM2ERR - 1)) / DITHER_ITEM2ERR;
				use.a[k] = KC_ClampV(v);
			}
			if (kc_distance(&kd3->ks[opx], &use) >= kd3->xradius[opx])
				opx = kd3_closest_transformed(kd3, &use, NULL);
			new_data[j] = opx;
			histogram[opx]++;

		  /* calculate and propagate the error between desired and selected color.
		     Assume that, with a large scale (1024), we don't need to worry about
		     image artifacts caused by error accumulation (the fact that the
		     error terms might not sum to the error). */
			for(int k = 0; k < 3; k++) {
				int v = (int)(use.a[k] - kd3->ks[opx].a[k]) * DITHER_ITEM2ERR;
				if (v) {
					err[_0x_][x + d0].a[k] += ((v * 7) & ~15) / 16;
					err[_1x_][x + d2].a[k] += ((v * 3) & ~15) / 16;
					err[_1x_][x +  1].a[k] += ((v * 5) & ~15) / 16;
					err[_1x_][x + d0].a[k] += ( v      & ~15) / 16;
				}
			}
		}
		/* change dithering directions */
		if ((direct = -direct) == -1) {
			x = width - 1, d0 = 0, d2 = 2;
			j = ((unsigned)y + 2) * (unsigned)width - 1;
		} else {
			x = 0, d0 = 2, d2 = 0;
			j = ((unsigned)y + 1) * (unsigned)width;
		}
		_0x_ ^= 1;
		_1x_ ^= 1;
	}
}

static unsigned char *make_floydstein_matrix_rand(unsigned seed)
{
	short i, *random_values = Gif_NewArray(short, N_RANDOM_VALUES);
	srand(seed);
	for (i = 0; i < N_RANDOM_VALUES; i++)
		random_values[i] = rand() % (DITHER_SCALE_M1 * 2) - DITHER_SCALE_M1;
	return (unsigned char *)random_values;
}

typedef struct ODSP_item { // odselect_plan_item
	unsigned char plan;
	unsigned short frac;
} ODSP_item;

static void plan_from_cplan(
	unsigned char *plan,
	unsigned       nplan,
	ODSP_item     *cp,
	unsigned       ncp,
	unsigned       whole
) {
	unsigned i, end_pos, cfsubt = 0, pos = 0;
	for (i = 0; i < ncp; i++) {
		end_pos = (cfsubt += cp[i].frac) * nplan / whole;
		while (pos != end_pos)
			plan[pos++] = cp[i].plan;
	}
	assert(pos == nplan);
}

static int ordered_plan_cmp(const unsigned char *a, const unsigned char *b, int *olums)
{
	int ia = (int)*a, ib = (int)*b;
	if (olums[ia] != olums[ib])
		ia = olums[ia], ib = olums[ib];
	return ia - ib;
}

static bool kc_line_closest(
	const kcolor *p0, const kcolor *p1,
	const kcolor *ref,
	double       *t,
	unsigned     *dist
) {
	wkcolor p01 = KC_Set(
		p1->a[0] - p0->a[0],
		p1->a[1] - p0->a[1],
		p1->a[2] - p0->a[2]
	), p0ref = KC_Set(
		ref->a[0] - p0->a[0],
		ref->a[1] - p0->a[1],
		ref->a[2] - p0->a[2]
	);

	unsigned den = (unsigned)(
		p01.a[0] * p01.a[0] +
		p01.a[1] * p01.a[1] +
		p01.a[2] * p01.a[2]
	);
	if (den == 0)
		return false;
  /* NB: We've run out of bits of precision. We can calculate the
     denominator in unsigned arithmetic, but the numerator might
     be negative, or it might be so large that it is unsigned.
     Calculate the numerator as a double. */
	double dt = (double)(
		p01.a[0] * p0ref.a[0] +
		p01.a[1] * p0ref.a[1] +
		p01.a[2] * p0ref.a[2]) / den;

	if (dt < 0 || dt > 1)
		return false;

	kcolor online = KC_Set(
		KC_ClampV((int)(p01.a[0] * dt) + p0->a[0]),
		KC_ClampV((int)(p01.a[1] * dt) + p0->a[1]),
		KC_ClampV((int)(p01.a[2] * dt) + p0->a[2])
	);

	*t = dt;
	*dist = kc_distance(&online, ref);
	return true;
}

static bool kc_plane_closest(
	const kcolor *p0, const kcolor *p1,
	const kcolor *p2, const kcolor *ref,
	double       *t,  unsigned     *dist
) {
	wkcolor p01 = KC_Set(
		p1->a[0] - p0->a[0],
		p1->a[1] - p0->a[1],
		p1->a[2] - p0->a[2]
	), p0ref = KC_Set(
		ref->a[0] - p0->a[0],
		ref->a[1] - p0->a[1],
		ref->a[2] - p0->a[2]
	), p02 = KC_Set(
		p2->a[0] - p0->a[0],
		p2->a[1] - p0->a[1],
		p2->a[2] - p0->a[2]
	);
	double n[3], pvec[3], det, qvec[3], u, v;

  /* Calculate the non-unit normal of the plane determined by the input
     colors (p0-p2) */
	n[0] = p01.a[1] * p02.a[2] - p01.a[2] * p02.a[1];
	n[1] = p01.a[2] * p02.a[0] - p01.a[0] * p02.a[2];
	n[2] = p01.a[0] * p02.a[1] - p01.a[1] * p02.a[0];

  /* Moeller-Trumbore ray tracing algorithm: trace a ray from `ref` along
     normal `n`; convert to barycentric coordinates to see if the ray
     intersects with the triangle. */
	pvec[0] = n[1] * p02.a[2] - n[2] * p02.a[1];
	pvec[1] = n[2] * p02.a[0] - n[0] * p02.a[2];
	pvec[2] = n[0] * p02.a[1] - n[1] * p02.a[0];

	det = pvec[0] * p01.a[0] + pvec[1] * p01.a[1] + pvec[2] * p01.a[2];
	if (fabs(det) <= 0.0001220703125) /* optimizer will take care of that */
		return false;
	det = 1 / det;

	u = ( p0ref.a[0] * pvec[0] +
	      p0ref.a[1] * pvec[1] +
	      p0ref.a[2] * pvec[2] ) * det;
	if (u < 0 || u > 1)
		return false;

	qvec[0] = p0ref.a[1]*p01.a[2] - p0ref.a[2]*p01.a[1];
	qvec[1] = p0ref.a[2]*p01.a[0] - p0ref.a[0]*p01.a[2];
	qvec[2] = p0ref.a[0]*p01.a[1] - p0ref.a[1]*p01.a[0];

	v = ( n[0] * qvec[0] + n[1] * qvec[1] + n[2] * qvec[2] ) * det;
	if (v < 0 || v > 1 || u + v > 1)
		return false;

  /* Now we know at there is a point in the triangle that is closer to
     `ref` than any point along its edges. Return the barycentric
     coordinates for that point and the distance to that point. */
	t[0] = u;
	t[1] = v;
	v = (p02.a[0] * qvec[0] + p02.a[1] * qvec[1] + p02.a[2] * qvec[2]) * det;
	*dist = (unsigned)(v * v * (n[0] * n[0] + n[1] * n[1] + n[2] * n[2]) + 0.5);
	return true;
}

static void limit_ordered_dither_plan(
	unsigned char *plan,
	unsigned      nplan,
	unsigned      nc,
	const kcolor  *want,
	kd3_tree      *kd3
) {
	unsigned dist, mindist = UINT32_MAX,
	 i, j, k, ncp, nbestcp = 0;

	double t[2];
	ODSP_item cp[256], bestcp[16];

	if (nc > 16)
		nc = 16;

	/* sort colors */
	cp[0].plan = plan[0];
	cp[0].frac = ncp = 1;
	for (i = 1; i < nplan; i++) {
		if (plan[i - 1] == plan[i]) {
			cp[ncp - 1].frac++;
		} else {
			cp[ncp  ].plan = plan[i];
			cp[ncp++].frac = 1;
		}
	}
	/* calculate plan */
	for (i = 0; i < ncp; i++) {
		/* check for closest single color */
		dist = kc_distance(&kd3->ks[cp[i].plan], want);
		if (dist < mindist) {
			bestcp[0].plan = cp[i].plan;
			bestcp[0].frac = KC_WHOLE;
			nbestcp = 1;
			mindist = dist;
		}
		for (j = i + 1; nc >= 2 && j < ncp; ++j) {
			/* check for closest blend of two colors */
			if (kc_line_closest(&kd3->ks[cp[i].plan],
				                &kd3->ks[cp[j].plan],
				                want, &t[0], &dist)
				&& dist < mindist) {
				bestcp[0].plan = cp[i].plan;
				bestcp[1].plan = cp[j].plan;
				bestcp[0].frac = KC_WHOLE -
					(bestcp[1].frac = (int)(KC_WHOLE * t[0]));
				nbestcp = 2;
				mindist = dist;
			}
			for (k = j + 1; nc >= 3 && k < ncp; k++) {
				/* check for closest blend of three colors */
				if (kc_plane_closest(&kd3->ks[cp[i].plan],
					                 &kd3->ks[cp[j].plan],
					                 &kd3->ks[cp[k].plan],
					                 want, &t[0], &dist)
					&& dist < mindist) {
					bestcp[0].plan = cp[i].plan;
					bestcp[1].plan = cp[j].plan;
					bestcp[2].plan = cp[k].plan;
					bestcp[0].frac = KC_WHOLE -
						(bestcp[1].frac = (int)(KC_WHOLE * t[0])) -
						(bestcp[2].frac = (int)(KC_WHOLE * t[1]));
					nbestcp = 3;
					mindist = dist;
				}
			}
		}
	}
	plan_from_cplan(plan, nplan, bestcp, nbestcp, KC_WHOLE);
}

static void set_ordered_dither_plan(
	unsigned char *plan,
	unsigned       nplan,
	unsigned       nc,
	Gif_Color     *gfc,
	kd3_tree      *kd3,
	int           *olums,

	Gif_ColorTransform *cot
) {
	unsigned i, d;
	wkcolor err      = KC_Set(0,0,0);
	kcolor cur, want = KC_Set8g(cot->GammaTab, gfc->R, gfc->G, gfc->B);

	if (kd3->transform)
		kd3->transform(&want);

	for (i = 0; i < nplan; i++) {
		cur.a[0] = KC_ClampV(want.a[0] + err.a[0]);
		cur.a[1] = KC_ClampV(want.a[1] + err.a[1]);
		cur.a[2] = KC_ClampV(want.a[2] + err.a[2]);
		d = (plan[i] = kd3_closest_transformed(kd3, &cur, NULL));
		err.a[0] += want.a[0] - kd3->ks[d].a[0];
		err.a[1] += want.a[1] - kd3->ks[d].a[1];
		err.a[2] += want.a[2] - kd3->ks[d].a[2];
	}
	qSortPerm(unsigned char, plan, nplan, ordered_plan_cmp, olums);

	if (nc < nplan && plan[0] != plan[nplan - 1]) {
		int ncp = 1;
		for (i = 1; i < nplan; i++)
			ncp += plan[i - 1] != plan[i];
		if (ncp > nc)
			limit_ordered_dither_plan(plan, nplan, nc, &want, kd3);
	}
	gfc->haspixel = 1;
}

static void pow2_ordered_dither(
	Gif_Image     *gfi,
	unsigned char *all_new_data,
	Gif_Colormap  *old_cm,
	kd3_tree      *kd3,
	unsigned      *histogram,
	const unsigned char *matrix,
	unsigned char *plan,
	int           *olums,

	Gif_ColorTransform *cot
) {
	int mws = 0, nplans = 0, i, x, y;
	while ((1 << mws) != matrix[0])
		mws++; /* nada */
	while ((1 << nplans) != matrix[2])
		nplans++; /* nada */

	for (y = 0; y < gfi->height; y++) {
		unsigned char *data, *new_data, *thisplan;
		data = gfi->img[y];
		new_data = all_new_data + y * (unsigned)gfi->width;

		for (x = 0; x < gfi->width; x++) {
			/* the transparent color never gets adjusted */
			if (data[x] != gfi->transparent) {
				thisplan = &plan[data[x] << nplans];
				if (!old_cm->col[data[x]].haspixel)
					set_ordered_dither_plan(thisplan, 1 << nplans, matrix[3],
					                        &old_cm->col[data[x]], kd3, olums, cot);
				i = matrix[4 +  ((x + gfi->left) & (matrix[0] - 1))
				             + (((y + gfi->top ) & (matrix[1] - 1)) << mws)];
				new_data[x] = thisplan[i];
				histogram[new_data[x]]++;
			}
		}
	}
}

static void colormap_image_ordered(
	Gif_Image     *gfi,
	unsigned char *all_new_data,
	Gif_Colormap  *old_cm,
	kd3_tree      *kd3,
	unsigned      *histogram,

	Gif_ColorTransform *cot
) {
	unsigned char mw = cot->dpMatrix[0], nplan = cot->dpMatrix[2],
	              mh = cot->dpMatrix[1], ncols = cot->dpMatrix[3];

	/* Written with reference to Joel Ylilouma's versions. */
	unsigned char *plan = Gif_NewArray(unsigned char, nplan * old_cm->ncol);
	int i, *olums = Gif_NewArray(int, kd3->nitems);
	unsigned short x, y;

	/* Initialize luminances, create luminance sorter */
	for (i = 0; i < kd3->nitems; i++)
		olums[i] = kc_luminance(&kd3->ks[i]);

	/* Do the image! */
	if ((mw & (mw - 1)) == 0 && (mh & (mh - 1)) == 0 && (nplan & (nplan - 1)) == 0) {
		pow2_ordered_dither(gfi, all_new_data, old_cm, kd3, histogram, cot->dpMatrix, plan, olums, cot);
	} else {
		for (y = 0; y < gfi->height; y++) {
			unsigned char *new_data, *thisplan;
			new_data = all_new_data + y * (unsigned)gfi->width;
			for (x = 0; x < gfi->width; x++) {
				unsigned char pix = gfi->img[y][x];
				/* the transparent color never gets adjusted */
				if (pix != gfi->transparent) {
					thisplan = &plan[nplan * pix];
					if (!old_cm->col[pix].haspixel)
						set_ordered_dither_plan(thisplan, nplan, ncols,
						                        &old_cm->col[pix], kd3, olums, cot);
					i = cot->dpMatrix[4 +  (x + gfi->left) % mw
					                    + ((y + gfi->top ) % mh) * mw];
					new_data[x] = thisplan[i];
					histogram[new_data[x]]++;
				}
			}
		}
	}
	/* delete temporary storage */
	Gif_DeleteArray(olums);
	Gif_DeleteArray(plan);
}

/* return value 1 means run the dither again */
static bool try_assign_transparency(
	Gif_Image     *gfi,
	Gif_Colormap  *old_cm,
	unsigned char *new_data,
	Gif_Colormap  *new_cm,
	int           *new_ncol,
	kd3_tree      *kd3,
	unsigned      *histogram
) {
	unsigned min_used;
	unsigned short x, y;
	int i, transparent = gfi->transparent;
	int new_transparent = -1;
	Gif_Color transp_value;

	if (transparent < 0)
		return false;

	if (old_cm)
		transp_value = old_cm->col[transparent];
	else
		Gif_SetColor(transp_value, 0, 0, 0);

  /* look for an unused pixel in the existing colormap; prefer the same color
     we had */
	for (i = 0; i < *new_ncol; i++) {
		if (histogram[i] == 0 && Gif_ColorEq(transp_value, new_cm->col[i])) {
			new_transparent = i;
			goto found;
		}
	}
	for (i = 0; i < *new_ncol; i++) {
		if (histogram[i] == 0) {
			new_transparent = i;
			goto found;
		}
	}
	/* try to expand the colormap */
	if (*new_ncol < 256) {
		assert(*new_ncol < new_cm->capacity);
		new_transparent = *new_ncol;
		new_cm->col[new_transparent] = transp_value;
		(*new_ncol)++;
		goto found;
	}

	/* not found: mark the least-frequently-used color as the new transparent
		color and return 1 (meaning 'dither again') */
	assert(*new_ncol == 256);
	min_used = UINT32_MAX;
	for (i = 0; i < 256; i++) {
		if (histogram[i] < min_used) {
			new_transparent = i;
			min_used = histogram[i];
		}
	}
	kd3_disable(kd3, new_transparent); /* mark it as unusable */
	return true;

 found:
	for (i = y = 0; y < gfi->height; y++) {
		for (x = 0; x < gfi->width;  x++, i++) {
			unsigned char pix = gfi->img[y][x];
			if (pix == transparent)
				new_data[i] = new_transparent;
		}
	}
	gfi->transparent = new_transparent;
	return false;
}


void Gif_FullQuantizeColors(Gif_Stream *gfs, Gif_Colormap *new_colmap, Gif_ColorTransform *cot, Gif_CompressInfo *gcinfo)
{
	kd3_tree kd3;
	Gif_Color *new_col = new_colmap->col;
	int i, j, new_ncol = new_colmap->ncol;
	bool compress_new_cm = true, new_gray = true;

	Gif_Colormap  *gst_cm = gfs->global;
	unsigned short gst_bg = gfs->background;

	/* make sure colormap has enough space */
	if (new_colmap->capacity < 256) {
		Gif_Color *x = Gif_NewArray(Gif_Color, 256);
		memcpy(x, new_col, sizeof(Gif_Color) * new_ncol);
		Gif_DeleteArray(new_col);
		new_colmap->col = new_col = x;
		new_colmap->capacity = 256;
	}
	assert(new_colmap->capacity >= 256);

	/* new_col[j].pixel == number of pixels with color j in the new image. */
	for (j = 0; j < 256; j++)
		new_col[j].pixel = 0;

	/* initialize kd3 tree */
	for (j = 0; j < new_ncol; j++) {
		if (new_col[j].R != new_col[j].G ||
			new_col[j].R != new_col[j].B) {
			new_gray = false;
			break;
		}
	}
	kd3_init_build(&kd3, new_gray ? kc_luminance_transform : NULL, new_colmap, cot);

	_dith_work_fn do_DitherTransform = (
		cot->dither_plan == DiP_Posterize      ? colormap_image_posterize :
		cot->dither_plan == DiP_FloydSteinberg ? colormap_image_floyd_steinberg :
		/*  ordered dither plan */               colormap_image_ordered
	);
	for (i = 0; i < gfs->nimages; i++) {

		Gif_Image    *gfi  =  gfs->images[i];
		Gif_Colormap *gfcm =  gfi->local ?: gst_cm;
		bool was_compress  = !gfi->img;

		if (gfcm) {
			/* If there was an old colormap, change the image data */
			unsigned char *new_data = Gif_NewArray(unsigned char, (unsigned)gfi->width * (unsigned)gfi->height);
			unsigned histogram[256];

			/* Initialize colors */
			unmark_pixels(new_colmap);
			unmark_pixels(gfcm);

			if (was_compress)
				Gif_UncompressImage(gfs, gfi);

			kd3_enable_all(&kd3);
			do {
				for (j = 0; j < 256; j++)
					histogram[j] = 0;
				do_DitherTransform(gfi, new_data, gfcm, &kd3, histogram, cot);
			} while (try_assign_transparency(gfi, gfcm, new_data, new_colmap, &new_ncol,
											 &kd3, histogram));

			/* version 1.28 bug fix: release any compressed version or it'll cause bad images */
			Gif_ReleaseCompressedImage(gfi);
			Gif_SetUncompressedImage(gfi, false, new_data);

			/* update count of used colors */
			for (j = 0; j < 256; j++)
				new_col[j].pixel += histogram[j];
			if (gfi->transparent >= 0)
				/* we don't have data on the number of used colors for transparency so fudge it. */
				new_col[gfi->transparent].pixel += (unsigned)gfi->width * (unsigned)gfi->height / 8;
		} else {
			/* Can't compress new_cm afterwards if we didn't actively change colors over */
			compress_new_cm = false;
		}

		if (gfi->local) {
			Gif_DeleteColormap(gfi->local);
			gfi->local = NULL;
		}

		/* 1.92: recompress *after* deleting the local colormap */
		if (gfcm && was_compress) {
			Gif_FullCompressImage(gfs, gfi, gcinfo);
			Gif_ReleaseUncompressedImage(gfi);
		}
	}

  /* Set new_colmap->ncol from new_ncol. We didn't update new_colmap->ncol before so
     the closest-color algorithms wouldn't see any new transparent colors.
     That way added transparent colors were only used for transparency. */
	new_colmap->ncol = new_ncol;

	/* change the background. I hate the background by now */
	if (gst_cm && gst_bg < gst_cm->ncol && (!gfs->nimages || gfs->images[0]->transparent < 0)) {
		gfs->background = kd3_closest8g(&kd3, gst_cm->col[gst_bg], cot);
		new_col[gst_bg].pixel++;
	}
	else if (gfs->nimages > 0 && gfs->images[0]->transparent >= 0)
		gfs->background = gfs->images[0]->transparent;
	else
		gfs->background = 0;

	Gif_DeleteColormap(gfs->global);
	kd3_cleanup(&kd3);

	/* We may have used only a subset of the colors in new_colmap. We try to store
	   only that subset, just as if we'd piped the output of 'gifsicle
	   --use-colormap=X' through 'gifsicle' another time. */
	if (Gif_CopyColormap(gfs->global = Gif_New(Gif_Colormap), new_colmap))
		unmark_pixels(gfs->global);

	if (compress_new_cm) {
		/* only bother to recompress if we'll get anything out of it */
		compress_new_cm = false;
		for (j = 0; j < new_ncol - 1; j++) {
			if (new_col[j].pixel == 0 || new_col[j].pixel < new_col[j + 1].pixel) {
				compress_new_cm = true;
				break;
			}
		}
	}

	if (compress_new_cm) {
		int map[256];
		/* Gif_CopyColormap copies the 'pixel' values as well */
		new_col = gfs->global->col;
		for (j = 0; j < new_ncol; j++)
			new_col[j].haspixel = j;

		/* sort based on popularity */
		qsort(new_col, new_ncol, sizeof(Gif_Color), popularity_sort_compare);

		/* set up the map and reduce the number of colors */
		for (j = 0; j < new_ncol; j++)
			map[new_col[j].haspixel] = j;
		for (j = 0; j < new_ncol; j++) {
			if (!new_col[j].pixel) {
				gfs->global->ncol = j;
				break;
			}
		}
		/* map the image data, transparencies, and background */
		if (gfs->background < gfs->global->ncol)
			gfs->background = map[gfs->background];
		for (i = 0; i < gfs->nimages; i++) {

			Gif_Image *gfi    =  gfs->images[i];
			bool was_compress = !gfi->img;

			if (was_compress)
				Gif_UncompressImage(gfs, gfi);

			const unsigned size = (unsigned)gfi->width * (unsigned)gfi->height;
			unsigned char *data = gfi->image_data;

			for (j = 0; j < size; j++)
				data[j] = map[data[j]];
			if (gfi->transparent >= 0)
				gfi->transparent = map[gfi->transparent];

			if (was_compress) {
				Gif_FullCompressImage(gfs, gfi, gcinfo);
				Gif_ReleaseUncompressedImage(gfi);
			}
		}
	}
}


/* 
 * Halftone algorithms
 */
typedef struct halftone_pixelinfo {
	int x, y;
	double distance, angle;
} halftone_pixelinfo;

static halftone_pixelinfo *halftone_pixel_make(int w, int h)
{
	int x, y, k;
	halftone_pixelinfo *hp = Gif_NewArray(halftone_pixelinfo, w * h);
	for (y = k = 0; y < h; y++) {
		for (x = 0; x < w; x++, k++) {
			hp[k].x = x;
			hp[k].y = y;
			hp[k].distance = -1;
		}
	}
	return hp;
}

static void halftone_pixel_combine(halftone_pixelinfo *hp, double cx, double cy)
{
	double n_y = (double)hp->y - cy,
	       n_x = (double)hp->x - cx,
	       n_d = n_x * n_x + n_y * n_y;
	if (hp->distance < 0 || n_d < hp->distance) {
		hp->distance = n_d;
		hp->angle = atan2(n_y, n_x);
	}
}

static int halftone_pixel_compare(const void *va, const void *vb)
{
	const halftone_pixelinfo *a = (const halftone_pixelinfo *)va;
	const halftone_pixelinfo *b = (const halftone_pixelinfo *)vb;
	if (fabs(a->distance - b->distance) > 0.01)
		return a->distance < b->distance ? -1 : 1;
	else
		return a->angle < b->angle ? -1 : 1;
}

static unsigned char *halftone_pixel_matrix(halftone_pixelinfo *hp, int w, int h, int nc)
{
	int i;
	unsigned count = w * h;
	unsigned char *m = Gif_NewArray(unsigned char, 4 + count);
	m[0] = w;
	m[1] = h;
	m[3] = nc;
	if (count > 255) {
		double s = 255. / count;
		m[2] = 255;
		for (i = 0; i < count; i++)
			m[4 + hp[i].x + hp[i].y * w] = (int)(i * s);
	} else {
		m[2] = count;
		for (i = 0; i < count; i++)
			m[4 + hp[i].x + hp[i].y * w] = i;
	}
	Gif_DeleteArray(hp);
	return m;
}

static unsigned char *make_halftone_matrix_square(int w, int h, int nc)
{
	halftone_pixelinfo *hp = halftone_pixel_make(w, h);
	int i, size = w * h;
	for (i = 0; i < size; i++)
		halftone_pixel_combine(&hp[i], (w - 1) / 2.0, (h - 1) / 2.0);
	qsort(hp, size, sizeof(*hp), halftone_pixel_compare);
	return halftone_pixel_matrix(hp, w, h, nc);
}

static unsigned char *make_halftone_matrix_triangular(int w, int h, int nc)
{
	halftone_pixelinfo *hp = halftone_pixel_make(w, h);
	int i, size = w * h;
	for (i = 0; i < size; i++) {
		halftone_pixel_combine(&hp[i],(w - 1) / 2.0, (h - 1) / 2.0);
		halftone_pixel_combine(&hp[i],    -0.5,    -0.5);
		halftone_pixel_combine(&hp[i], w - 0.5,    -0.5);
		halftone_pixel_combine(&hp[i],    -0.5, h - 0.5);
		halftone_pixel_combine(&hp[i], w - 0.5, h - 0.5);
	}
	qsort(hp, size, sizeof(*hp), halftone_pixel_compare);
	return halftone_pixel_matrix(hp, w, h, nc);
}


bool Gif_InitColorTransform(Gif_ColorTransform *cot)
{
	if (!cot) return false;
	cot->dpMatrix    = NULL;
	cot->dither_plan = DiP_Posterize;
	cot->GammaTab    = srgb_gamma_table_256;
	cot->RGammaTab   = srgb_revgamma_table_256;
	cot->gamma_range = 2.2;
	cot->gamma_type  = GK_SRGB;
	return true;
}

void Gif_FreeColorTransform(Gif_ColorTransform *cot)
{
	if (cot->dpMatrix)
		Gif_DeleteArray(cot->dpMatrix);
	if (cot->GammaTab != srgb_gamma_table_256)
		Gif_DeleteArray(cot->GammaTab);
	if (cot->RGammaTab != srgb_revgamma_table_256)
		Gif_DeleteArray(cot->RGammaTab);
}

void Gif_SetDitherPlan(
	Gif_ColorTransform *cot,
	Gif_Dither plan,
	unsigned char w,
	unsigned char h,
	unsigned int ncol
) {
	if (plan == DiP_FloydSteinberg) {
		cot->dpMatrix = make_floydstein_matrix_rand(w * h + ncol);
	} else
	if (plan == DiP_SquareHalftone) {
		cot->dpMatrix = make_halftone_matrix_square(w, h, ncol);
	} else
	if (plan == DiP_TriangleHalftone) {
		cot->dpMatrix = make_halftone_matrix_triangular(w, h, ncol);
	} else {
		const unsigned size = (
			plan == DiP_3x3_Ordered     ? DiMx_3X3_SIZE :
			plan == DiP_4x4_Ordered     ? DiMx_4X4_SIZE :
			plan == DiP_64x64_ReOrdered ? DiMx_64X64_SIZE : DiMx_8X8_SIZE
		);
		unsigned char *matrix = Gif_NewArray(unsigned char, size);

		memcpy(matrix, (
			plan == DiP_3x3_Ordered ? Dither_Matrix_o3x3 :
			plan == DiP_4x4_Ordered ? Dither_Matrix_o4x4 :
			plan == DiP_8x8_Ordered ? Dither_Matrix_o8x8 :
			plan == DiP_45_Diagonal ? Dither_Matrix_diagonal45_8 :
			plan == DiP_64x64_ReOrdered ? Dither_Matrix_ro64x64 : Dither_Matrix_halftone8
		), size);

		if (ncol >= 2 && h > 1 && h != matrix[3])
			matrix[3] = h;
		cot->dpMatrix = matrix;
	}
	cot->dither_plan = plan;
}

void Gif_SetGamma(Gif_ColorTransform *cot, Gif_Gamma type, double range) {
#if HAVE_POW
	if (type == cot->gamma_type && (type != GK_Numeric || range == cot->gamma_range))
		return;
	bool is_srgb_g = cot->GammaTab == srgb_gamma_table_256,
	     is_srgb_rg = cot->RGammaTab == srgb_revgamma_table_256;
	if (type == GK_SRGB) {
		if (!is_srgb_g) {
			Gif_FreeArray(cot->GammaTab);
			cot->GammaTab = srgb_gamma_table_256;
		}
		if (!is_srgb_rg) {
			Gif_FreeArray(cot->RGammaTab);
			cot->RGammaTab = srgb_revgamma_table_256;
		}
	} else {
		unsigned short * gamma_tab = is_srgb_g  ? Gif_NewArray(unsigned short, 256) : (unsigned short *)cot->GammaTab;
		unsigned short *rgamma_tab = is_srgb_rg ? Gif_NewArray(unsigned short, 256) : (unsigned short *)cot->RGammaTab;

		for (int j = 0; j < 256; j++) {
			 gamma_tab[j] = (int)(pow(j / 255.0,     range) * INT16_MAX + 0.5);
			rgamma_tab[j] = (int)(pow(j / 256.0, 1 / range) * INT16_MAX + 0.5);
		/* The gamma_tab[j]++ ensures that round-trip gamma correction
		   always preserve the input colors. Without it, one might have,
		   for example, input values 0, 1, and 2 all mapping to
		   gamma-corrected value 0. Then a round-trip through gamma
		   correction loses information. */
			while (j > 0 &&  gamma_tab[j] <=  gamma_tab[j - 1] &&  gamma_tab[j] < INT16_MAX)
				 gamma_tab[j]++;
			while (j > 0 && rgamma_tab[j] <= rgamma_tab[j - 1] && rgamma_tab[j] < INT16_MAX)
				rgamma_tab[j]++;
		}
		cot->GammaTab  =  gamma_tab;
		cot->RGammaTab = rgamma_tab;
	}
#endif
	cot->gamma_type  = type;
	cot->gamma_range = range;
}

