/* kcolor.h - Color-oriented function declarations for gifsicle.
   Copyright (C) 2013-2017 Eddie Kohler, ekohler@gmail.com
   This file is part of gifsicle.

   Gifsicle is free software. It is distributed under the GNU Public License,
   version 2; you can copy, distribute, or alter it at will, as long
   as this notice is kept intact and this source code is made available. There
   is no warranty, express or implied. */

#ifndef KCOLOR_H
#define KCOLOR_H
#include <gifsi.h>
#include <assert.h>
#include "unipart.h"

/* kcolor: a 3D vector, each component has 15 bits of precision
   15 bits means KC_MAX * KC_MAX always fits within a signed 32-bit
   integer, and a 3-D squared distance always fits within an unsigned 32-bit
   integer. */
#define KC_MAX     0x7FFF
#define KC_WHOLE   0x8000
#define KC_HALF    0x4000
#define KC_QUARTER 0x2000
#define KC_BITS    15

typedef struct kcolor {
	short a[3];
} kcolor;

#define KC_ClampV(v) (_MAX(0, _MIN((v), KC_MAX)))
/* set `a0/a1/a2` (RGB) */
#define KC_Set(r,g,b) { .a = {r,g,b} }
/* set the gamma transformation of `a0/a1/a2` (RGB) */
#define KC_Set8g(gmt,r,g,b) { .a = {gmt[r], gmt[g], gmt[b]} }

/* return kcolor gamma transformation of `gcl` */
static inline kcolor kc_Make8g(const Gif_Color gcl, const Gif_ColorTransform *cot)
{
	kcolor kc = KC_Set8g(cot->GammaTab, gcl.R, gcl.G, gcl.B);
	return kc;
}

/* return reverse gamma transformation of `a` */
static inline unsigned char kc_revgamma_transform(const short a, const Gif_ColorTransform *cot)
{
	unsigned short c = cot->RGammaTab[a >> 7];
	while (c < 0x7F80 && a >= cot->GammaTab[(c + 0x80) >> 7])
		c += 0x80;
	return c >> 7;
}

/* return reverse gamma transformation `gcl` */
static inline Gif_Color kc_MColR8g(const kcolor k, const Gif_ColorTransform *cot)
{
	Gif_Color gcl = {
		.haspixel = 0,
		.R = kc_revgamma_transform(k.a[0], cot),
		.G = kc_revgamma_transform(k.a[1], cot),
		.B = kc_revgamma_transform(k.a[2], cot)
	};
	return gcl;
};

/* return the squared Euclidean distance between `*x` and `*y` */
static inline unsigned kc_distance(const kcolor* x, const kcolor* y) {
	/* It’s OK to use unsigned multiplication for this: the low 32 bits
	are the same either way. Unsigned avoids undefined behavior. */
	unsigned d0 = x->a[0] - y->a[0];
	unsigned d1 = x->a[1] - y->a[1];
	unsigned d2 = x->a[2] - y->a[2];
	return d0 * d0 + d1 * d1 + d2 * d2;
}

/* return the luminance value for `*x`; result is between 0 and KC_MAX */
static inline int kc_luminance(const kcolor* x) {
	return (55 * x->a[0] + 183 * x->a[1] + 19 * x->a[2]) >> 8;
}

/* set `*x` to the grayscale version of `*x`, transformed by luminance */
static inline void kc_luminance_transform(kcolor* x) {
 /* For grayscale colormaps, use distance in luminance space instead of
    distance in RGB space. The weights for the R,G,B components in
    luminance space are 0.2126,0.7152,0.0722. (That's ITU primaries, which
    are compatible with sRGB; NTSC recommended our previous values,
    0.299,0.587,0.114.) Using the proportional factors 55,183,19 we get a
    scaled gray value between 0 and 255 * 257; dividing by 256 gives us
    what we want. Thanks to Christian Kumpf, <kumpf@igd.fhg.de>, for
    providing a patch.*/
	x->a[0] = x->a[1] = x->a[2] = kc_luminance(x);
}


typedef union kacolor {
	kcolor k;
	short a[4];
#if HAVE_INT64_T
	long long q; /* to get better alignment */
#endif
} kacolor;

/* set `a0/a1/a2/a3` (RGBA) */
#define KA_Set(r,g,b,t) { .a = { r,g,b,t } };
/* set `a0/a1/a2/a3` (RGBA) to `0` */
#define KA_Clear(ka) (ka.a[0] = ka.a[1] = ka.a[2] = ka.a[3] = 0)

/* wkcolor: like kcolor, but components are 32 bits instead of 16 */
typedef struct wkcolor {
	int a[3];
} wkcolor;

/* kd3_tree: kd-tree for 3 dimensions, indexing kcolors */
typedef struct kd3_tree kd3_tree;
typedef struct kd3_treepos kd3_treepos;

struct kd3_treepos {
	int offset, pivot;
};
struct kd3_tree {
	kd3_treepos* tree;
	int ntree;
	int disabled;
	kcolor* ks;
	int nitems;
	int items_cap;
	int maxdepth;
	void (*transform)(kcolor*);
	unsigned* xradius;
};

/* initialize `kd3` with the given color `transform` (may be NULL) */
void kd3_init(kd3_tree* kd3, void (*transform)(kcolor*));

/* free `kd3` */
void kd3_cleanup(kd3_tree* kd3);

/* add the transformed color `k` to `*kd3` (do not apply `kd3->transform`). */
void kd3_add_transformed(kd3_tree* kd3, const kcolor* k);

/* given 8-bit color `a0/a1/a2` (RGB), gamma-transform it, transform it
   by `kd3->transform` if necessary, and add it to `*kd3` */
static inline void kd3_add8g(kd3_tree *kd3, const Gif_Color gfc, const Gif_ColorTransform *cot)
{
	kcolor k = KC_Set8g(cot->GammaTab, gfc.R, gfc.G, gfc.B);

	if (kd3->transform)
		kd3->transform(&k);
	kd3_add_transformed(kd3, &k);
}

/* set `kd3->xradius`. given color `i`, `kd3->xradius[i]` is the square of the
   color's uniquely owned neighborhood.
   If `kc_distance(&kd3->ks[i], &k) < kd3->xradius[i]`, then
   `kd3_closest_transformed(kd3, &k) == i`. */
void kd3_build_xradius(kd3_tree* kd3);

/* build the actual kd-tree for `kd3`. must be called before kd3_closest. */
void kd3_build(kd3_tree* kd3);

/* kd3_init + kd3_add8g for all colors in `gfcm` + kd3_build */
void kd3_init_build(kd3_tree* kd3, void (*transform)(kcolor*),
                    const Gif_Colormap* gfcm, const Gif_ColorTransform *cot);

/* return the index of the color in `*kd3` closest to `k`.
   if `dist!=NULL`, store the distance from `k` to that index in `*dist`. */
int kd3_closest_transformed(kd3_tree* kd3, const kcolor* k,
                            unsigned* dist);

/* given 8-bit color `a0/a1/a2` (RGB), gamma-transform it, transform it by
   `kd3->transform` if necessary, and return the index of the color in
   `*kd3` closest to it. */
static inline int kd3_closest8g(kd3_tree *kd3, const Gif_Color gcl, const Gif_ColorTransform *cot)
{
	kcolor k = KC_Set8g(cot->GammaTab, gcl.R, gcl.G, gcl.B);

	if (kd3->transform)
		kd3->transform(&k);
	return kd3_closest_transformed(kd3, &k, NULL);
}

/* disable color index `i` in `*kd3`: it will never be returned by
   `kd3_closest*` */
static inline void kd3_disable(kd3_tree* kd3, int i) {
	assert((unsigned) i < (unsigned) kd3->nitems);
	assert(kd3->disabled < 0 || kd3->disabled == i);
	kd3->disabled = i;
}

/* enable all color indexes in `*kd3` */
static inline void kd3_enable_all(kd3_tree* kd3) {
	kd3->disabled = -1;
}


typedef struct kchistitem {
	kacolor ka;
	unsigned count;
} kchistitem;

typedef struct kchist {
	kchistitem* h;
	int n;
	int capacity;
} kchist;

kchistitem *
     kchist_add     ( kchist *, kcolor      , unsigned   );
void kchist_init    ( kchist * );
void kchist_cleanup ( kchist * );
void kchist_compress( kchist * );


typedef struct {
	kchist* kch;
	int* closest;
	unsigned* min_dist;
	unsigned* min_dither_dist;
	int* chosen;
	int nchosen;
} kcdiversity;

void kcdiversity_init        ( kcdiversity *, kchist *, int );
void kcdiversity_cleanup     ( kcdiversity * );
int  kcdiversity_find_popular( kcdiversity * );
int  kcdiversity_find_diverse( kcdiversity *, double );
int  kcdiversity_choose      ( kcdiversity *, int, int );


#if WITH_SIMD && HAVE_VECTOR_SIZE_VECTOR_TYPES
typedef float float4 __attribute__((vector_size (sizeof(float) * 4)));
typedef int     int4 __attribute__((vector_size (sizeof(int  ) * 4)));
#elif WITH_SIMD && HAVE_EXT_VECTOR_TYPE_VECTOR_TYPES
typedef float float4 __attribute__((ext_vector_type (4)));
#else
typedef float float4[4];
#endif

typedef union scale_color {
	float4 a;
} scale_color;

static inline scale_color sc_MakeKC(const kcolor k) {
	scale_color sc = KA_Set(k.a[0], k.a[1], k.a[2], KC_MAX);
	return sc;
}

#if WITH_SIMD
# define SCVEC_ADDV(  sc, sc2   ) (sc).a += (sc2).a
# define SCVEC_MULV(  sc, sc2   ) (sc).a *= (sc2).a
# define SCVEC_MULF(  sc,      f) (sc).a *= (f)
# define SCVEC_DIVF(  sc,      f) (sc).a /= (f)
# define SCVEC_ADDVxF(sc, sc2, f) (sc).a += (sc2).a * (f)
# if HAVE___BUILTIN_SHUFFLEVECTOR
#  define SCVEC_ROT3(out, sc) do { (out).a = __builtin_shufflevector((sc).a, (sc).a, 1, 2, 0, 3); } while (0)
# else
#  define SCVEC_ROT3(out, sc) do { int4 shufmask__ = {1, 2, 0, 3}; (out).a = __builtin_shuffle((sc).a, shufmask__); } while (0)
# endif
#else
# define SCVEC_FOREACH(t) do { int k__; for (k__ = 0; k__ != 4; ++k__) { t; } } while (0)
# define SCVEC_ADDV(  sc, sc2   ) SCVEC_FOREACH((sc).a[k__] += (sc2).a[k__])
# define SCVEC_MULV(  sc, sc2   ) SCVEC_FOREACH((sc).a[k__] *= (sc2).a[k__])
# define SCVEC_MULF(  sc,      f) SCVEC_FOREACH((sc).a[k__] *= (f))
# define SCVEC_DIVF(  sc,      f) SCVEC_FOREACH((sc).a[k__] /= (f))
# define SCVEC_ADDVxF(sc, sc2, f) SCVEC_FOREACH((sc).a[k__] += (sc2).a[k__] * (f))
# define SCVEC_ROT3(out, sc) do { float __a0 = (sc).a[0]; (out).a[0] = (sc).a[1]; (out).a[1] = (sc).a[2]; (out).a[2] = __a0; (out).a[3] = (sc).a[3]; } while (0)
#endif

#endif // KCOLOR_H
