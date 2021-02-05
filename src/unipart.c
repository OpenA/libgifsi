#include <stdlib.h>
#include <string.h>

#include "unipart.h"

/* An alternative to qsort, with an identical interface.
   This code is part of the GNU C Library.
   Copyright (C) 1992-2020 Free Software Foundation, Inc.
   Written by Mike Haertel, September 1988.

   LGPL 2.1 <https://www.gnu.org/licenses/>. */

struct msort_param {
	unsigned size;
	_scmp_fn cmp;
	void    *arg;
	char    *tmp;
};

static void msort_with_tmp(const struct msort_param *p, void *b, unsigned n)
{
	if (n <= 1)
		return;

	unsigned n1 = n / 2,
	         n2 = n - n1;
	char    *b1 = b,
	        *b2 = (char *)b + (n1 * p->size);

	msort_with_tmp(p, b1, n1);
	msort_with_tmp(p, b2, n2);

	const unsigned lf = p->size;
	void    *arg = p->arg;
	char    *tmp = p->tmp;
	_scmp_fn cmp = p->cmp;

	while (n1 > 0 && n2 > 0)
	{
		if ((*cmp)(b1, b2, arg) <= 0) {
			memcpy(tmp, b1, lf);
			tmp += lf;
			b1  += lf;
			n1--;
		} else {
			memcpy(tmp, b2, lf);
			tmp += lf;
			b2  += lf;
			n2--;
		}
	}
	if (n1 > 0)
		memcpy(tmp, b1, n1 * lf);
	memcpy(b, p->tmp, (n - n2) * lf);
}

void __qsort_r(void *arr, unsigned len, unsigned size, _scmp_fn cmp, void *arg)
{
	char *tmp = malloc(len * size);
	struct msort_param p = {
		.size = size,
		.cmp  = cmp,
		.arg  = arg,
		.tmp  = tmp
	};
	msort_with_tmp(&p, arr, len);
	free(tmp);
}
