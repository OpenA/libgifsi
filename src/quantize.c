/* quantize.c - Histograms and quantization for gifsicle.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#include "kmatrix.h"
#include "gifsicle.h"
#include "kcolor.h"

#include <limits.h>
#include <ctype.h>
#include <math.h>
#include <time.h>

unsigned short *gamma_tables[2] = {
	(unsigned short *)srgb_gamma_table_256,
	(unsigned short *)srgb_revgamma_table_256};

/* return the gamma transformation of `*gfc` */
kcolor kc_Make8g(const Gif_Color gfc)
{
	kcolor kc = KC_Set8g(gamma_tables[0], gfc.R, gfc.G, gfc.B);
	return kc;
}

static inline void unmark_pixels(Gif_Colormap *gfcm)
{
	for (int i = 0; i < gfcm->ncol; i++)
		gfcm->col[i].haspixel = 0;
}

const char *kc_debug_str(kcolor x) {
	static int whichbuf = 0;
	static char buf[4][32];
	whichbuf = (whichbuf + 1) % 4;
	if (x.a[0] >= 0 && x.a[1] >= 0 && x.a[2] >= 0) {
		kc_revgamma_transform(&x);
		sprintf(buf[whichbuf], "#%02X%02X%02X",
				x.a[0] >> 7, x.a[1] >> 7, x.a[2] >> 7);
	} else
		sprintf(buf[whichbuf], "<%d,%d,%d>", x.a[0], x.a[1], x.a[2]);
	return buf[whichbuf];
}

void kc_set_gamma(int type, double gamma) {
#if HAVE_POW
	static int cur_type = KC_GAMMA_SRGB;
	static double cur_gamma = 2.2;
	int i, j;
	if (type == cur_type && (type != KC_GAMMA_NUMERIC || gamma == cur_gamma))
		return;
	if (type == KC_GAMMA_SRGB) {
		if (gamma_tables[0] != srgb_gamma_table_256) {
			Gif_DeleteArray(gamma_tables[0]);
			Gif_DeleteArray(gamma_tables[1]);
		}
		gamma_tables[0] = (unsigned short *)srgb_gamma_table_256;
		gamma_tables[1] = (unsigned short *)srgb_revgamma_table_256;
	} else {
		if (gamma_tables[0] == srgb_gamma_table_256) {
			gamma_tables[0] = Gif_NewArray(unsigned short, 256);
			gamma_tables[1] = Gif_NewArray(unsigned short, 256);
		}
		for (j = 0; j != 256; j++) {
			gamma_tables[0][j] = (int)(pow(j / 255.0,     gamma) * INT16_MAX + 0.5);
			gamma_tables[1][j] = (int)(pow(j / 256.0, 1 / gamma) * INT16_MAX + 0.5);
		/* The gamma_tables[i][j]++ ensures that round-trip gamma correction
		   always preserve the input colors. Without it, one might have,
		   for example, input values 0, 1, and 2 all mapping to
		   gamma-corrected value 0. Then a round-trip through gamma
		   correction loses information. */
			for (i = 0; i != 2; i++)
				while (j && gamma_tables[i][j] <= gamma_tables[i][j - 1] && gamma_tables[i][j] < INT16_MAX)
					gamma_tables[i][j]++;
		}
	}
	cur_type  = type;
	cur_gamma = gamma;
#else
	(void)type, (void)gamma;
#endif
}

void kc_revgamma_transform(kcolor *x)
{
	for (int d = 0; d != 3; d++) {
		int c = gamma_tables[1][x->a[d] >> 7];
		while (c < 0x7F80 && x->a[d] >= gamma_tables[0][(c + 0x80) >> 7])
			c += 0x80;
		x->a[d] = c;
	}
}

#if 0
static void kc_test_gamma() {
	short x, y, z;
	for (x = 0; x < 256; x++)
		for (y = 0; y < 256; y++)
			for (z = 0; z < 256; z++) {
				kcolor k = KC_Set8g(gamma_tables[0], x, y, z);
				kc_revgamma_transform(&k);
				if ((k.a[0] >> 7) != x || (k.a[1] >> 7) != y || (k.a[2] >> 7) != z) {
					kcolor kg = KC_Set8g(gamma_tables[0], x, y, z);
					fprintf(stderr, "#%02X%02X%02X ->g #%04X%04X%04X ->revg #%02X%02X%02X!\n",
							x, y, z, kg.a[0], kg.a[1], kg.a[2],
							k.a[0] >> 7, k.a[1] >> 7, k.a[2] >> 7);
					assert(0);
				}
			}
}
#endif


/*
 * kchist methods and functions
 */
static const int kchist_sizes[] = {
	4093, 16381, 65521, 262139, 1048571, 4194301,
	16777213, 67108859, 268435459, 1073741839
};

static void kchist_grow(kchist *kch)
{
	kchistitem *old_h = kch->h;

	int i = 0, old_capacity = kch->capacity ?: kch->n;
	while (kchist_sizes[i] <= old_capacity)
		i++;/* do nothing */

	int new_capacity = kch->capacity = kchist_sizes[i];
	kch->h = Gif_NewArray(kchistitem, new_capacity);
	kch->n = 0;
	for (i = 0; i < new_capacity; i++)
		kch->h[i].count = 0;
	for (i = 0; i < old_capacity; i++) {
		if (old_h[i].count)
			kchist_add(kch, old_h[i].ka.k, old_h[i].count);
	}
	Gif_DeleteArray(old_h);
}

void kchist_init(kchist *kch)
{
	int i, capacity = kch->capacity = kchist_sizes[0];
	kch->h = Gif_NewArray(kchistitem, capacity);
	kch->n = 0;
	for (i = 0; i < capacity; i++)
		kch->h[i].count = 0;
}

void kchist_cleanup(kchist *kch)
{
	Gif_DeleteArray(kch->h);
	kch->h = NULL;
}

kchistitem *kchist_add(kchist *kch, kcolor k, unsigned count)
{
	unsigned hash1, hash2 = 0;
	kacolor ka;
	kchistitem *khi;
	ka.k = k;
	ka.a[3] = 0;

	if (!kch->capacity || kch->n > ((kch->capacity * 3) >> 4))
		kchist_grow(kch);

	hash1 = (
	  ((ka.a[0] & 0x7FE0) << 15) |
	  ((ka.a[1] & 0x7FE0) << 5 ) |
	  ((ka.a[2] & 0x7FE0) >> 5)) % kch->capacity;

	while (1) {
		khi = &kch->h[hash1];
		if (!khi->count || memcmp(&khi->ka, &ka, sizeof(ka)) == 0)
			break;
		if (!hash2) {
			hash2 = (
			  ((ka.a[0] & 0x03FF) << 20) |
			  ((ka.a[1] & 0x03FF) << 10) |
			   (ka.a[2] & 0x03FF)) % kch->capacity ?: 1;
		}
		hash1 += hash2;
		if (hash1 >= (unsigned)kch->capacity)
			hash1 -= kch->capacity;
	}

	if (!khi->count) {
		khi->ka = ka;
		kch->n++;
	}
	khi->count += count;
	if (khi->count < count)
		khi->count = UINT32_MAX;
	return khi;
}

void kchist_compress(kchist *kch)
{
	int i, j;
	for (i = 0, j = kch->n; i < kch->n;) {
		if (kch->h[i].count)
			i++;
		else if (kch->h[j].count) {
			kch->h[i] = kch->h[j];
			i++, j++;
		} else
			j++;
	}
	kch->capacity = 0;
}

void kchist_make(kchist *kch, Gif_Stream *gfs, unsigned *ntransp_store)
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
					kchist_add(kch, kc_Make8g(cm->col[c]), count[c]);
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
				kchist_add(kch, kc_Make8g(gcm->col[c]), gcount[c]);
		}
	}
	/* now, make the linear histogram from the hashed histogram */
	kchist_compress(kch);
	*ntransp_store = transp_count;
}

static int red_kchistitem_compare(const void *va, const void *vb)
{
	const kchistitem *a = (const kchistitem *)va;
	const kchistitem *b = (const kchistitem *)vb;
	return a->ka.a[0] - b->ka.a[0];
}

static int green_kchistitem_compare(const void *va, const void *vb)
{
	const kchistitem *a = (const kchistitem *)va;
	const kchistitem *b = (const kchistitem *)vb;
	return a->ka.a[1] - b->ka.a[1];
}

static int blue_kchistitem_compare(const void *va, const void *vb)
{
	const kchistitem *a = (const kchistitem *)va;
	const kchistitem *b = (const kchistitem *)vb;
	return a->ka.a[2] - b->ka.a[2];
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

typedef struct {
	int first, size;
	unsigned pixel;
} adaptive_slot;

Gif_Colormap *colormap_median_cut(kchist *kch, Gt_OutputData *od)
{
	int adapt_size = od->colormap_size;
	adaptive_slot *slots = Gif_NewArray(adaptive_slot, adapt_size);
	Gif_Colormap *gfcm = Gif_NewFullColormap(adapt_size, 256);
	Gif_Color *adapt = gfcm->col;
	int nadapt;
	int i, j, k;

	/* This code was written with reference to ppmquant by Jef Poskanzer,
       part of the pbmplus package. */

	if (adapt_size < 2 || adapt_size > 256)
		fatal_error("adaptive palette size must be between 2 and 256");
	if (adapt_size >= kch->n && !od->colormap_fixed)
		warning(1, "trivial adaptive palette (only %d %s in source)",
				kch->n, kch->n == 1 ? "color" : "colors");
	if (adapt_size >= kch->n)
		adapt_size = kch->n;

	/* 0. remove any transparent color from consideration; reduce adaptive
       palette size to accommodate transparency if it looks like that'll be
       necessary */
	if (adapt_size > 2 && adapt_size < kch->n && kch->n <= 265 && od->colormap_needs_transparency)
		adapt_size--;

	/* 1. set up the first slot, containing all pixels. */
	slots[0].first = 0;
	slots[0].size = kch->n;
	slots[0].pixel = 0;
	for (i = 0; i < kch->n; i++)
		slots[0].pixel += kch->h[i].count;

	/* 2. split slots until we have enough. */
	for (nadapt = 1; nadapt < adapt_size; nadapt++) {
		adaptive_slot *split = 0;
		kcolor minc, maxc;
		kchistitem *slice;

		/* 2.1. pick the slot to split. */
		unsigned split_pixel = 0;
		for (i = 0; i < nadapt; i++) {
			if (slots[i].size >= 2 && slots[i].pixel > split_pixel) {
				split = &slots[i];
				split_pixel = slots[i].pixel;
			}
		}
		if (!split)
			break;
		slice = &kch->h[split->first];

		/* 2.2. find its extent. */
		kchistitem *trav = slice;
		minc = maxc = trav->ka.k;
		for (i = 1, trav++; i < split->size; i++, trav++) {
			for (k = 0; k != 3; ++k) {
				minc.a[k] = _MIN(minc.a[k], trav->ka.a[k]);
				maxc.a[k] = _MAX(maxc.a[k], trav->ka.a[k]);
			}
		}

		/* 2.3. decide how to split it. use the luminance method. also sort
		   the colors. */
		{
			double red_diff = 0.299 * (maxc.a[0] - minc.a[0]);
			double green_diff = 0.587 * (maxc.a[1] - minc.a[1]);
			double blue_diff = 0.114 * (maxc.a[2] - minc.a[2]);
			if (red_diff >= green_diff && red_diff >= blue_diff)
				qsort(slice, split->size, sizeof(kchistitem), red_kchistitem_compare);
			else if (green_diff >= blue_diff)
				qsort(slice, split->size, sizeof(kchistitem), green_kchistitem_compare);
			else
				qsort(slice, split->size, sizeof(kchistitem), blue_kchistitem_compare);
		}

		/* 2.4. decide where to split the slot and split it there. */
		{
			unsigned half_pixels = split->pixel / 2;
			unsigned pixel_accum = slice[0].count;
			unsigned diff1, diff2;
			for (i = 1; i < split->size - 1 && pixel_accum < half_pixels; i++)
				pixel_accum += slice[i].count;

		/* We know the area before the split has more pixels than the
		   area after, possibly by a large margin (bad news). If it
		   would shrink the margin, change the split. */
			diff1 = 2 * pixel_accum - split->pixel;
			diff2 = split->pixel - 2 * (pixel_accum - slice[i - 1].count);
			if (diff2 < diff1 && i > 1) {
				i--;
				pixel_accum -= slice[i].count;
			}
			slots[nadapt].first = split->first + i;
			slots[nadapt].size = split->size - i;
			slots[nadapt].pixel = split->pixel - pixel_accum;
			split->size = i;
			split->pixel = pixel_accum;
		}
	}

	/* 3. make the new palette by choosing one color from each slot. */
	for (i = 0; i < nadapt; i++) {
		double px[3] = { 0, 0, 0 };
		kchistitem *slice = &kch->h[slots[i].first];
		kcolor kc;
		for (j = 0; j < slots[i].size; j++) {
			px[0] += slice[j].ka.a[0] * (double)slice[j].count;
			px[1] += slice[j].ka.a[1] * (double)slice[j].count;
			px[2] += slice[j].ka.a[2] * (double)slice[j].count;
		}
		kc.a[0] = (int)(px[0] / slots[i].pixel);
		kc.a[1] = (int)(px[1] / slots[i].pixel);
		kc.a[2] = (int)(px[2] / slots[i].pixel);
		adapt[i] = kc_togfcg(&kc);
	}

	Gif_DeleteArray(slots);
	gfcm->ncol = nadapt;
	return gfcm;
}

void kcdiversity_init(kcdiversity *div, kchist *kch, int dodither)
{
	int i;
	div->kch = kch;
	qsort(kch->h, kch->n, sizeof(kchistitem), popularity_kchistitem_compare);
	div->closest = Gif_NewArray(int, kch->n);
	div->min_dist = Gif_NewArray(unsigned, kch->n);
	for (i = 0; i != kch->n; ++i)
		div->min_dist[i] = (unsigned)-1;
	if (dodither) {
		div->min_dither_dist = Gif_NewArray(unsigned, kch->n);
		for (i = 0; i < kch->n; i++)
			div->min_dither_dist[i] = (unsigned)-1;
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
	int i, j, k, n = div->kch->n;
	kchistitem *hist = div->kch->h;
	int *chosenmap = Gif_NewArray(int, n);
	scale_color *di = Gif_NewArray(scale_color, div->nchosen);
	for (i = 0; i < div->nchosen; i++) {
		di[i].a[0] = di[i].a[1] = di[i].a[2] = di[i].a[3] = 0;
		chosenmap[div->chosen[i]] = i;
	}
	for (i = 0; i < n; i++) {
		double count = hist[i].count;
		if (div->closest[i] == i)
			count *= 3;
		j = chosenmap[div->closest[i]];
		for (k = 0; k != 3; k++)
			di[j].a[k] += hist[i].ka.a[k] * count;
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
	Gif_DeleteArray(chosenmap);
	Gif_DeleteArray(di);
}

static Gif_Colormap *colormap_diversity(kchist *kch, Gt_OutputData *od, int blend)
{
	int adapt_size = od->colormap_size;
	kcdiversity div;
	Gif_Colormap *gfcm = Gif_NewFullColormap(adapt_size, 256);
	int nadapt = 0;
	int chosen;

  /* This code was uses XV's modified diversity algorithm, and was written
     with reference to XV's implementation of that algorithm by John Bradley
     <bradley@cis.upenn.edu> and Tom Lane <Tom.Lane@g.gp.cs.cmu.edu>. */

	if (adapt_size < 2 || adapt_size > 256)
		fatal_error("adaptive palette size must be between 2 and 256");
	if (adapt_size > kch->n && !od->colormap_fixed)
		warning(1, "trivial adaptive palette (only %d colors in source)", kch->n);
	if (adapt_size > kch->n)
		adapt_size = kch->n;

  /* 0. remove any transparent color from consideration; reduce adaptive
     palette size to accommodate transparency if it looks like that'll be
     necessary */
  /* It will be necessary to accommodate transparency if (1) there is
     transparency in the image; (2) the adaptive palette isn't trivial; and
     (3) there are a small number of colors in the image (arbitrary constant:
     <= 265), so it's likely that most images will use most of the slots, so
     it's likely there won't be unused slots. */
	if (adapt_size > 2 && adapt_size < kch->n && kch->n <= 265 && od->colormap_needs_transparency)
		adapt_size--;

	/* blending has bad effects when there are very few colors */
	if (adapt_size < 4)
		blend = 0;

	/* 1. initialize min_dist and sort the colors in order of popularity. */
	kcdiversity_init(&div, kch, od->dither_type != dither_none);

	/* 2. choose colors one at a time */
	for (nadapt = 0; nadapt < adapt_size; nadapt++) {
		/* 2.1. choose the color to be added */
		if (nadapt == 0 || (nadapt >= 10 && nadapt % 2 == 0))
			/* 2.1a. want most popular unchosen color */
			chosen = kcdiversity_find_popular(&div);
		else if (od->dither_type == dither_none)
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
		kcdiversity_choose(&div, chosen, nadapt > 0 && nadapt < 64 && od->dither_type != dither_none);
	}
	/* 3. make the new palette by choosing one color from each slot. */
	if (blend)
		colormap_diversity_do_blend(&div);

	for (nadapt = 0; nadapt < div.nchosen; nadapt++)
		gfcm->col[nadapt] = kc_togfcg(&kch->h[div.chosen[nadapt]].ka.k);
	gfcm->ncol = nadapt;

	kcdiversity_cleanup(&div);
	return gfcm;
}

Gif_Colormap *colormap_blend_diversity(kchist *kch, Gt_OutputData *od)
{
	return colormap_diversity(kch, od, 1);
}

Gif_Colormap *colormap_flat_diversity(kchist *kch, Gt_OutputData *od)
{
	return colormap_diversity(kch, od, 0);
}

Gif_Colormap *Gif_ColormapDiversity(Gif_Stream *gfs, unsigned adapt_size, unsigned div_type, Gif_DitherPlan *dp) {
	
	Gif_Colormap *gfcm = Gif_NewColormap(adapt_size, 256);
	unsigned ntransp;
	kchist kch;

	kchist_make(&kch, gfs, &ntransp);

	if (adapt_size > kch.n)
		adapt_size = kch.n;

	if (adapt_size > 2 && adapt_size < kch.n && kch.n <= 265 && ntransp > 0)
		adapt_size--;
	
	if (div_type == GIF_DIVERSITY_MEDIAN_CUT) {
		//colormap_median_cut();
	} else {

	}
}


/*
 * kd_tree allocation and deallocation
 */
struct kd3_treepos {
	int offset, pivot;
};

void kd3_init(kd3_tree *kd3, void (*transform)(kcolor *))
{
	kd3->tree = NULL;
	kd3->ks = Gif_NewArray(kcolor, 256);
	kd3->nitems = 0;
	kd3->items_cap = 256;
	kd3->transform = transform;
	kd3->xradius = NULL;
	kd3->disabled = -1;
}

void kd3_cleanup(kd3_tree *kd3)
{
	Gif_DeleteArray(kd3->tree);
	Gif_DeleteArray(kd3->ks);
	Gif_DeleteArray(kd3->xradius);
}

void kd3_add_transformed(kd3_tree *kd3, const kcolor *k)
{
	if (kd3->nitems == kd3->items_cap) {
		kd3->items_cap *= 2;
		Gif_ReArray(kd3->ks, kcolor, kd3->items_cap);
	}
	kd3->ks[kd3->nitems++] = *k;
	if (kd3->tree) {
		Gif_DeleteArray(kd3->tree);
		Gif_DeleteArray(kd3->xradius);
		kd3->tree = NULL;
		kd3->xradius = NULL;
	}
}

void kd3_add8g(kd3_tree *kd3, const Gif_Color gfc)
{
	kcolor k = KC_Set8g(gamma_tables[0], gfc.R, gfc.G, gfc.B);

	if (kd3->transform)
		kd3->transform(&k);
	kd3_add_transformed(kd3, &k);
}

static int kd3_item_0_cmp  (const int *a, const int *b, kd3_tree *kd3) { return kd3->ks[*a].a[0] - kd3->ks[*b].a[0]; }
static int kd3_item_1_cmp  (const int *a, const int *b, kd3_tree *kd3) { return kd3->ks[*a].a[1] - kd3->ks[*b].a[1]; }
static int kd3_item_2_cmp  (const int *a, const int *b, kd3_tree *kd3) { return kd3->ks[*a].a[2] - kd3->ks[*b].a[2]; }
static int kd3_item_all_cmp(const int *a, const int *b, kd3_tree *kd3) { return memcmp(&kd3->ks[*a], &kd3->ks[*b], sizeof(kcolor)); }

static int kd3_build_range(kd3_tree *kd3, int *perm, int nperm, int n, int depth)
{
	int m, nleft, nright, aix = depth % 3;

	if (depth > kd3->maxdepth)
		kd3->maxdepth = depth;

	while (n >= kd3->ntree) {
		kd3->ntree *= 2;
		Gif_ReArray(kd3->tree, kd3_treepos, kd3->ntree);
	}
	if (nperm <= 1) {
		kd3->tree[n].pivot = (nperm == 0 ? -1 : perm[0]);
		kd3->tree[n].offset = -1;
		return 2;
	}
	qSortPerm(int, perm, nperm, (
	    aix == 1 ? kd3_item_1_cmp :
	    aix == 2 ? kd3_item_2_cmp :
	  /*aix == 0*/ kd3_item_0_cmp ), kd3);

	/* pick pivot: a color component to split */
	for (m = nperm >> 1; m > 0; m--) {
		if (kd3->ks[perm[m]].a[aix] != kd3->ks[perm[m - 1]].a[aix])
			break;
	}
	/* don't split entirely to the right (infinite loop) */
	if (m == 0) {
		 /* also, don't split entirely to the left */
		for (m = nperm >> 1; m < nperm - 1; m++) {
			if (kd3->ks[perm[m]].a[aix] != kd3->ks[perm[m - 1]].a[aix])
				break;
		}
	}
	kd3->tree[n].pivot = m == 0 ?
		kd3->ks[perm[m    ]].a[aix] :
		kd3->ks[perm[m - 1]].a[aix] + (
			(kd3->ks[perm[m]].a[aix] - kd3->ks[perm[m - 1]].a[aix]) >> 1);

	/* recurse */
	nleft  = kd3_build_range(kd3, perm, m, n + 1, depth + 1);
	kd3->tree[n].offset = ++nleft; // increase nleft by 1
	nright = kd3_build_range(kd3, &perm[m], nperm - m, n + nleft, depth + 1);
	return nleft + nright;
}

#if 0
static void kd3_print_depth(kd3_tree* kd3, int depth, kd3_treepos* p,
                            int* a, int* b) {
    int i;
    char x[6][10];
    for (i = 0; i != 3; ++i) {
        if (a[i] == INT_MIN)
            sprintf(x[2*i], "*");
        else
            sprintf(x[2*i], "%d", a[i]);
        if (b[i] == INT_MAX)
            sprintf(x[2*i+1], "*");
        else
            sprintf(x[2*i+1], "%d", b[i]);
    }
    printf("%*s<%s:%s,%s:%s,%s:%s>", depth*3, "",
           x[0], x[1], x[2], x[3], x[4], x[5]);
    if (p->offset < 0) {
        if (p->pivot >= 0) {
            assert(kd3->ks[p->pivot].a[0] >= a[0]);
            assert(kd3->ks[p->pivot].a[1] >= a[1]);
            assert(kd3->ks[p->pivot].a[2] >= a[2]);
            assert(kd3->ks[p->pivot].a[0] < b[0]);
            assert(kd3->ks[p->pivot].a[1] < b[1]);
            assert(kd3->ks[p->pivot].a[2] < b[2]);
            printf(" ** @%d: <%d,%d,%d>\n", p->pivot, kd3->ks[p->pivot].a[0], kd3->ks[p->pivot].a[1], kd3->ks[p->pivot].a[2]);
        }
    } else {
        int aindex = depth % 3, x[3];
        assert(p->pivot >= a[aindex]);
        assert(p->pivot < b[aindex]);
        printf((aindex == 0 ? " | <%d,_,_>\n" :
                aindex == 1 ? " | <_,%d,_>\n" : " | <_,_,%d>\n"), p->pivot);
        memcpy(x, b, sizeof(int) * 3);
        x[aindex] = p->pivot;
        kd3_print_depth(kd3, depth + 1, p + 1, a, x);
        memcpy(x, a, sizeof(int) * 3);
        x[aindex] = p->pivot;
        kd3_print_depth(kd3, depth + 1, p + p->offset, x, b);
    }
}

static void kd3_print(kd3_tree* kd3) {
    int a[3], b[3];
    a[0] = a[1] = a[2] = INT_MIN;
    b[0] = b[1] = b[2] = INT_MAX;
    kd3_print_depth(kd3, 0, kd3->tree, a, b);
}
#endif

void kd3_build_xradius(kd3_tree *kd3)
{
	/* create xradius */
	if (kd3->xradius)
		return;
	unsigned i, j, dist, *xrad = Gif_NewArray(unsigned, kd3->nitems);

	for (i = 0; i < kd3->nitems; i++)
		xrad[i] = UINT32_MAX;
	for (i = 0; i < kd3->nitems; i++) {
		for (j = i + 1; j < kd3->nitems; j++) {
			dist = kc_distance(&kd3->ks[i], &kd3->ks[j]) / 4;
			if (dist < xrad[i])
				xrad[i] = dist;
			if (dist < xrad[j])
				xrad[j] = dist;
		}
	}
	kd3->xradius = xrad;
}

void kd3_build(kd3_tree* kd3)
{
	int i, d, *perm;
	assert(!kd3->tree);

	/* create tree */
	kd3->tree = Gif_NewArray(kd3_treepos, 256);
	kd3->ntree = 256;
	kd3->maxdepth = 0;

	/* create copy of items; remove duplicates */
	perm = Gif_NewArray(int, kd3->nitems);
	for (i = 0; i < kd3->nitems; i++)
		perm[i] = i;
	qSortPerm(int, perm, kd3->nitems, kd3_item_all_cmp, kd3);

	for (i = 0, d = 1; i + d < kd3->nitems; i++) {
		if (!memcmp(&kd3->ks[perm[i]], &kd3->ks[perm[i + d]], sizeof(kcolor)))
			d++, i--;
		else if (d > 1)
			perm[i + 1] = perm[i + d];
	}
	kd3_build_range(kd3, perm, kd3->nitems - (d - 1), 0, 0);
	assert(kd3->maxdepth < 32);
	Gif_DeleteArray(perm);
}

void kd3_init_build(kd3_tree *kd3, void (*transform)(kcolor *), const Gif_Colormap *gfcm)
{
	kd3_init(kd3, transform);
	for (int i = 0; i < gfcm->ncol; i++)
		kd3_add8g(kd3, gfcm->col[i]);
	kd3_build(kd3);
}

int kd3_closest_transformed(kd3_tree *kd3, const kcolor *k, unsigned *dist_store)
{
	const kd3_treepos *stack[32];
	unsigned char state[32];
	int stackpos = 0;
	int result = -1;
	unsigned mindist = (unsigned)-1;

	if (!kd3->tree)
		kd3_build(kd3);

	stack[0] = kd3->tree;
	state[0] = 0;

	while (stackpos >= 0) {
		const kd3_treepos *p;
		assert(stackpos < 32);
		p = stack[stackpos];

		if (p->offset < 0)
		{
			if (p->pivot >= 0 && kd3->disabled != p->pivot) {
				unsigned dist = kc_distance(&kd3->ks[p->pivot], k);
				if (dist < mindist) {
					mindist = dist;
					result = p->pivot;
				}
			}
			if (--stackpos >= 0)
				++state[stackpos];
		}
		else if (state[stackpos] == 0)
		{
			if (k->a[stackpos % 3] < p->pivot)
				stack[stackpos + 1] = p + 1;
			else
				stack[stackpos + 1] = p + p->offset;
			++stackpos;
			state[stackpos] = 0;
		}
		else
		{
			int delta = k->a[stackpos % 3] - p->pivot;
			if (state[stackpos] == 1 && (unsigned)delta * (unsigned)delta < mindist)
			{
				if (delta < 0)
					stack[stackpos + 1] = p + p->offset;
				else
					stack[stackpos + 1] = p + 1;
				++stackpos;
				state[stackpos] = 0;
			}
			else if (--stackpos >= 0)
				++state[stackpos];
		}
	}
	if (dist_store)
		*dist_store = mindist;
	return result;
}

int kd3_closest8g(kd3_tree *kd3, const Gif_Color gfc)
{
	kcolor k = KC_Set8g(gamma_tables[0], gfc.R, gfc.G, gfc.B);

	if (kd3->transform)
		kd3->transform(&k);
	return kd3_closest_transformed(kd3, &k, NULL);
}


void colormap_image_posterize(
	Gif_Image     *gfi,
	unsigned char *new_data,
	Gif_Colormap  *old_cm,
	kd3_tree      *kd3,
	unsigned      *histogram,
	Gif_DitherPlan *dp
) {
	Gif_Color *col = old_cm->col;
	int i, ncol = old_cm->ncol;
	int map[256];
	int transparent = gfi->transparent;
	unsigned short x, y;

	/* find closest colors in new colormap */
	for (i = 0; i < ncol; i++) {
		map[i] = col[i].pixel = kd3_closest8g(kd3, col[i]);
		col[i].haspixel = 1;
	}

	/* map image */
	for (i = y = 0; y < gfi->height; y++) {
		for (x = 0; x < gfi->width;  x++, i++) {
			unsigned char pix = gfi->img[y][x];
			if (pix != transparent)
				++histogram[(new_data[i] = map[pix])];
		}
	}
}

#define DITHER_SCALE    1024
#define DITHER_SHIFT    10
#define DITHER_SCALE_M1 (DITHER_SCALE - 1)
#define DITHER_ITEM2ERR (1 << (DITHER_SHIFT - 7))
#define N_RANDOM_VALUES 512

void colormap_image_floyd_steinberg(
	Gif_Image     *gfi,
	unsigned char *all_new_data,
	Gif_Colormap  *old_cm,
	kd3_tree      *kd3,
	unsigned      *histogram,
	Gif_DitherPlan *dp
) {
	static int *random_values = 0;

	int width = gfi->width;
	int dither_direction = 0;
	int transparent = gfi->transparent;
	int i, j, k;
	wkcolor *err, *err1;

	/* Initialize distances */
	for (i = 0; i < old_cm->ncol; i++) {
		Gif_Color *c = &old_cm->col[i];
		c->pixel = kd3_closest8g(kd3, old_cm->col[i]);
		c->haspixel = 1;
	}
  /* This code was written with reference to ppmquant by Jef Poskanzer, part
     of the pbmplus package. */

  /* Initialize Floyd-Steinberg error vectors to small random values, so we
     don't get artifacts on the top row */
	err  = Gif_NewArray(wkcolor, width + 2);
	err1 = Gif_NewArray(wkcolor, width + 2);
  /* Use the same random values on each call in an attempt to minimize
    "jumping dithering" effects on animations */
	if (!random_values) {
		srand(time(NULL)); // init random number generator
		random_values = Gif_NewArray(int, N_RANDOM_VALUES);
		for (i = 0; i < N_RANDOM_VALUES; i++)
			random_values[i] = rand() % (DITHER_SCALE_M1 * 2) - DITHER_SCALE_M1;
	}
	for (i = 0; i < gfi->width + 2; i++) {
		j = (i + gfi->left) * 3;
		for (k = 0; k < 3; ++k)
			err[i].a[k] = random_values[(j + k) % N_RANDOM_VALUES];
	}
	/* err1 initialized below */

	kd3_build_xradius(kd3);

	/* Do the image! */
	for (j = 0; j < gfi->height; j++) {
		int d0, d1, d2, d3; /* used for error diffusion */
		unsigned char *data, *new_data;
		int x;

		if (dither_direction) {
			x = width - 1;
			d0 = 0, d1 = 2, d2 = 1, d3 = 0;
		} else {
			x = 0;
			d0 = 2, d1 = 0, d2 = 1, d3 = 2;
		}
		data = &gfi->img[j][x];
		new_data = all_new_data + j * (unsigned)width + x;

		for (i = 0; i < width + 2; i++) {
			err1[i].a[0] = err1[i].a[1] = err1[i].a[2] = 0;
		}
		/* Do a single row */
		while (x >= 0 && x < width) {
			int e;

			/* the transparent color never gets adjusted */
			if (*data == transparent)
				goto next;

			/* find desired new color */
			Gif_Color old_c = old_cm->col[*data];
			kcolor use = KC_Set8g(gamma_tables[0],
				old_c.R, old_c.G, old_c.B
			);
			if (kd3->transform)
				kd3->transform(&use);
			/* use Floyd-Steinberg errors to adjust */
			for (k = 0; k < 3; k++) {
				int v = use.a[k] + (err[x + 1].a[k] & ~(DITHER_ITEM2ERR - 1)) / DITHER_ITEM2ERR;
				use.a[k] = KC_ClampV(v);
			}
			e = old_c.pixel;
			if (kc_distance(&kd3->ks[e], &use) < kd3->xradius[e])
				*new_data = e;
			else
				*new_data = kd3_closest_transformed(kd3, &use, NULL);
			histogram[*new_data]++;

		  /* calculate and propagate the error between desired and selected color.
		     Assume that, with a large scale (1024), we don't need to worry about
		     image artifacts caused by error accumulation (the fact that the
		     error terms might not sum to the error). */
			for (k = 0; k < 3; k++) {
				e = (use.a[k] - kd3->ks[*new_data].a[k]) * DITHER_ITEM2ERR;
				if (e) {
					err [x + d0].a[k] += ((e * 7) & ~15) / 16;
					err1[x + d1].a[k] += ((e * 3) & ~15) / 16;
					err1[x + d2].a[k] += ((e * 5) & ~15) / 16;
					err1[x + d3].a[k] += ( e      & ~15) / 16;
				}
			}

		next:
			if (dither_direction)
				x--, data--, new_data--;
			else
				x++, data++, new_data++;
		}
		/* Did a single row */

		/* change dithering directions */
		{
			wkcolor *temp = err1;
			err1 = err;
			err = temp;
			dither_direction = !dither_direction;
		}
	}
	/* delete temporary storage */
	Gif_DeleteArray(err);
	Gif_DeleteArray(err1);
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
	int           *olums
) {
	unsigned i, d;
	wkcolor err      = KC_Set(0,0,0);
	kcolor cur, want = KC_Set8g(gamma_tables[0], gfc->R, gfc->G, gfc->B);

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
	int           *olums
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
					                        &old_cm->col[data[x]], kd3, olums);
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
	Gif_DitherPlan *dp
) {
	unsigned char mw = dp->matrix[0], nplan = dp->matrix[2],
	              mh = dp->matrix[1], ncols = dp->matrix[3];

	/* Written with reference to Joel Ylilouma's versions. */
	unsigned char *plan = Gif_NewArray(unsigned char, nplan * old_cm->ncol);
	int i, *olums = Gif_NewArray(int, kd3->nitems);
	unsigned short x, y;

	/* Initialize luminances, create luminance sorter */
	for (i = 0; i < kd3->nitems; i++)
		olums[i] = kc_luminance(&kd3->ks[i]);

	/* Do the image! */
	if ((mw & (mw - 1)) == 0 && (mh & (mh - 1)) == 0 && (nplan & (nplan - 1)) == 0) {
		pow2_ordered_dither(gfi, all_new_data, old_cm, kd3, histogram, dp->matrix, plan, olums);
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
						                        &old_cm->col[pix], kd3, olums);
					i = dp->matrix[4 +  (x + gfi->left) % mw
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


void Gif_FullQuantizeColors(Gif_Stream *gfs, Gif_Colormap *new_cm, Gif_DitherPlan *dp)
{
	kd3_tree kd3;
	Gif_Color *new_col = new_cm->col;
	int new_ncol = new_cm->ncol;
	unsigned i, j;
	bool compress_new_cm = true, new_gray = true;

	/* make sure colormap has enough space */
	if (new_cm->capacity < 256) {
		Gif_Color *x = Gif_NewArray(Gif_Color, 256);
		memcpy(x, new_col, sizeof(Gif_Color) * new_ncol);
		Gif_DeleteArray(new_col);
		new_cm->col = new_col = x;
		new_cm->capacity = 256;
	}
	assert(new_cm->capacity >= 256);

	/* new_col[j].pixel == number of pixels with color j in the new image. */
	for (j = 0; j < 256; j++)
		new_col[j].pixel = 0;

	/* initialize kd3 tree */
	for (j = 0; j < new_cm->ncol; j++) {
		if (new_col[j].R != new_col[j].G ||
			new_col[j].R != new_col[j].B) {
			new_gray = false;
			break;
		}
	}
	kd3_init_build(&kd3, new_gray ? kc_luminance_transform : NULL, new_cm);

	for (i = 0; i < gfs->nimages; i++) {

		Gif_Image    *gfi  =  gfs->images[i];
		Gif_Colormap *gfcm =  gfi->local ?: gfs->global;
		bool was_compress  = !gfi->img;

		if (gfcm) {
			/* If there was an old colormap, change the image data */
			unsigned char *new_data = Gif_NewArray(unsigned char, (unsigned)gfi->width * (unsigned)gfi->height);
			unsigned histogram[256];

			/* Initialize colors */
			unmark_pixels(new_cm);
			unmark_pixels(gfcm);

			if (was_compress)
				Gif_UncompressImage(gfs, gfi);

			kd3_enable_all(&kd3);
			do {
				for (j = 0; j < 256; j++)
					histogram[j] = 0;
				dp->doWork(gfi, new_data, gfcm, &kd3, histogram, dp);
			} while (try_assign_transparency(gfi, gfcm, new_data, new_cm, &new_ncol,
											 &kd3, histogram));

			Gif_ReleaseUncompressedImage(gfi);
			/* version 1.28 bug fix: release any compressed version or it'll cause bad images */
			Gif_ReleaseCompressedImage(gfi);
			Gif_SetUncompressedImage(gfi, new_data, Gif_Free, false);

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
			Gif_FullCompressImage(gfs, gfi, &gif_write_info);
			Gif_ReleaseUncompressedImage(gfi);
		}
	}

  /* Set new_cm->ncol from new_ncol. We didn't update new_cm->ncol before so
     the closest-color algorithms wouldn't see any new transparent colors.
     That way added transparent colors were only used for transparency. */
	new_cm->ncol = new_ncol;

	/* change the background. I hate the background by now */
	if ((gfs->nimages == 0 || gfs->images[0]->transparent < 0) && gfs->global && gfs->background < gfs->global->ncol) {
		gfs->background = kd3_closest8g(&kd3, gfs->global->col[gfs->background]);
		new_col[gfs->background].pixel++;
	}
	else if (gfs->nimages > 0 && gfs->images[0]->transparent >= 0)
		gfs->background = gfs->images[0]->transparent;
	else
		gfs->background = 0;

	Gif_DeleteColormap(gfs->global);
	kd3_cleanup(&kd3);

	/* We may have used only a subset of the colors in new_cm. We try to store
	   only that subset, just as if we'd piped the output of 'gifsicle
	   --use-colormap=X' through 'gifsicle' another time. */
	unmark_pixels(
		(gfs->global = Gif_CopyColormap(new_cm))
	);

	if (compress_new_cm) {
		/* only bother to recompress if we'll get anything out of it */
		compress_new_cm = false;
		for (j = 0; j < new_cm->ncol - 1; j++) {
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
		for (j = 0; j < new_cm->ncol; j++)
			new_col[j].haspixel = j;

		/* sort based on popularity */
		qsort(new_col, new_cm->ncol, sizeof(Gif_Color), popularity_sort_compare);

		/* set up the map and reduce the number of colors */
		for (j = 0; j < new_cm->ncol; j++)
			map[new_col[j].haspixel] = j;
		for (j = 0; j < new_cm->ncol; j++) {
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
				Gif_FullCompressImage(gfs, gfi, &gif_write_info);
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


void Gif_InitDitherPlan(Gif_DitherPlan *dp, Gif_Dither type, unsigned char w, unsigned char h, unsigned ncol)
{
	dp->matrix = NULL;

	if (type == DiP_Posterize) {
		dp->doWork = (_dith_work_fn)colormap_image_posterize;
	} else
	if (type == DiP_FloydSteinberg) {
		dp->doWork = (_dith_work_fn)colormap_image_floyd_steinberg;
	} else
	if (type == DiP_SquareHalftone) {
		dp->doWork = (_dith_work_fn)colormap_image_ordered;
		dp->matrix = make_halftone_matrix_square(w, h, ncol);
	} else
	if (type == DiP_TriangleHalftone) {
		dp->doWork = (_dith_work_fn)colormap_image_ordered;
		dp->matrix = make_halftone_matrix_triangular(w, h, ncol);
	} else {
		const unsigned size = (
			type == DiP_3x3_Ordered     ? DiMx_3X3_SIZE :
			type == DiP_4x4_Ordered     ? DiMx_4X4_SIZE :
			type == DiP_64x64_ReOrdered ? DiMx_64X64_SIZE : DiMx_8X8_SIZE
		);

		dp->doWork = (_dith_work_fn)colormap_image_ordered;
		dp->matrix = Gif_NewArray(unsigned char, size);

		memcpy(dp->matrix, (
			type == DiP_3x3_Ordered ? Dither_Matrix_o3x3 :
			type == DiP_4x4_Ordered ? Dither_Matrix_o4x4 :
			type == DiP_8x8_Ordered ? Dither_Matrix_o8x8 :
			type == DiP_45_Diagonal ? Dither_Matrix_diagonal45_8 :
			type == DiP_64x64_ReOrdered ? Dither_Matrix_ro64x64 : Dither_Matrix_halftone8
		), size);

		if (ncol >= 2 && h > 1 && h != dp->matrix[3])
			dp->matrix[3] = h;
	}
}

void Gif_FreeDitherPlan(Gif_DitherPlan *dp)
{
	Gif_DeleteArray(dp->matrix);
	dp->doWork = NULL;
}

