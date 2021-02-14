/*
   This is not related part of libgifsi.
   Most of the code in unipart.c comes from other free open source projects.
*/
#ifndef UNIPART_H
#define UNIPART_H

#ifndef UINT32_MAX
# define UINT32_MAX 0xFFFFFFFFU
# define UINT16_MAX 0xFFFFU
#endif

#ifndef INT32_MAX
# define INT32_MAX 0x7FFFFFFF
# define INT16_MAX 0x7FFF
#endif

#define _MIN(a, b) ((a) < (b) ? (a) : (b))
#define _MAX(a, b) ((a) > (b) ? (a) : (b))

typedef int(*_scmp_fn)(const void*, const void*, void*);

void __qsort_r(void*, unsigned, unsigned, _scmp_fn, void*);

#define qSortPerm(_T, b, n, cmp, arg) __qsort_r(b, n, sizeof(_T), (_scmp_fn)cmp, arg);

#define _SWAP_MEM(tmp, a, b, msize)\
	memcpy(tmp, a, msize);\
	memcpy(a, b, msize);\
	memcpy(b, tmp, msize)

#define _SWAP_PTR(_T, ptr_1, ptr_2) {\
	_T *t_ptr = ptr_1;\
		ptr_1 = ptr_2;\
		ptr_2 = t_ptr;\
}

#endif // UNIPART_H
