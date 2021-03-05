
#include "kcolor.h"


#ifdef DEBUG_MODE

/* return a hex color string definition for `x` */
const char *kc_debug_str(kcolor x, Gif_ColorTransform *cot) {
	static int whichbuf = 0;
	static char buf[4][32];
	whichbuf = (whichbuf + 1) % 4;
	if (x.a[0] >= 0 && x.a[1] >= 0 && x.a[2] >= 0) {
		sprintf(buf[whichbuf], "#%02X%02X%02X",
				kc_revgamma_transform(x.a[0], cot),
				kc_revgamma_transform(x.a[1], cot),
				kc_revgamma_transform(x.a[2], cot));
	} else
		sprintf(buf[whichbuf], "<%d,%d,%d>", x.a[0], x.a[1], x.a[2]);
	return buf[whichbuf];
}

static void kc_test_gamma(Gif_ColorTransform *cot) {
	short x, y, z;
	for (x = 0; x < 256; x++)
		for (y = 0; y < 256; y++)
			for (z = 0; z < 256; z++) {
				kcolor k = KC_Set8g(cot->GammaTab, x, y, z);
				unsigned char rg0 = kc_revgamma_transform(k.a[0], cot),
				              rg1 = kc_revgamma_transform(k.a[1], cot),
				              rg2 = kc_revgamma_transform(k.a[2], cot);
				if (rg0 != x || rg1 != y || rg2 != z) {
					fprintf(stderr, "#%02X%02X%02X ->g #%04X%04X%04X ->revg #%02X%02X%02X!\n",
							x, y, z, k.a[0], k.a[1], k.a[2], rg0, rg1, rg2);
					assert(0);
				}
			}
}
#endif



/*
 * kd_tree allocation and deallocation
 */
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
	}
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

void kd3_init_build(kd3_tree *kd3, void (*transform)(kcolor *), const Gif_Colormap *gfcm, const Gif_ColorTransform *cot)
{
	kd3_init(kd3, transform);
	for (int i = 0; i < gfcm->ncol; i++)
		kd3_add8g(kd3, gfcm->col[i], cot);
	kd3_build(kd3);
}

int kd3_closest_transformed(kd3_tree *kd3, const kcolor *k, unsigned *dist_store)
{
	const kd3_treepos *stack[32];
	unsigned char state[32];
	int stackpos = 0;
	int result = -1;
	unsigned mindist = UINT32_MAX;

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

#ifdef DEBUG_MODE
static void kd3_print_depth(kd3_tree *kd3, int depth, kd3_treepos *p, int *a, int *b) {
	char x[6][10];
	for (int j = 0, i = 0; i < 3; i++, j = 2 * i) {
		if (a[i] == -INT32_MAX)
			sprintf(x[j], "*");
		else
			sprintf(x[j], "%d", a[i]);
		if (b[i] == INT32_MAX)
			sprintf(x[j+1], "*");
		else
			sprintf(x[j+1], "%d", b[i]);
	}
	printf("%*s<%s:%s,%s:%s,%s:%s>", depth * 3, "", x[0], x[1], x[2], x[3], x[4], x[5]);
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
static void kd3_print(kd3_tree *kd3) {
	int a[3], b[3];
	a[0] = a[1] = a[2] = -INT32_MAX;
	b[0] = b[1] = b[2] =  INT32_MAX;
	kd3_print_depth(kd3, 0, kd3->tree, a, b);
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
