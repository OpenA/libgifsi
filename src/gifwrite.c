/* -*- mode: c; c-basic-offset: 2 -*- */
/* gifwrite.c - Functions to write GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of the LCDF GIF library.

   The LCDF GIF library is free software. It is distributed under the GNU
   General Public License, version 2; you can copy, distribute, or alter it at
   will, as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied. */

#include <gifsi.h>
#include <assert.h>
#include "unipart.h"

#define WRITE_CODE_MAX     0x1000
#define WRITE_CODE_BITS    12
#define WRITE_BUFFER_SIZE  255

/* 1.Aug.1999 - Removed code hashing in favor of an adaptive tree strategy
   based on Whirlgif-3.04, written by Hans Dinsen-Hansen <dino@danbbs.dk>. Mr.
   Dinsen-Hansen brought the adaptive tree strategy to my attention and argued
   at length that it was better than hashing. In fact, he was right: it runs a
   lot faster. However, it does NOT create "better" results in any way.

   Each code is represented by a Node. The Nodes form a tree with variable
   fan-out -- up to 'clear_code' children per Node. There are two kinds of
   Node, TABLE and LINKS. In a TABLE node, the children are stored in a table
   indexed by suffix -- thus, it's very efficient to determine a given child.
   In a LINKS node, the existent children are stored in a linked list. This is
   slightly slower to access. When a LINKS node gets more than
   'MAX_LINKS_TYPE-1' children, it is converted to a TABLE node. (This is why
   it's an adaptive tree.)

   Problems with this implementation: MAX_LINKS_TYPE is fixed, so GIFs with
   very small numbers of colors (2, 4, 8) won't get the speed benefits of
   TABLE nodes. */

#define TABLE_TYPE      0
#define LINKS_TYPE      1
#define MAX_LINKS_TYPE  5

typedef unsigned char  PixB_t;
typedef unsigned short Code_t;
typedef struct  Node_t Node_t;

struct Node_t {
	Code_t  code;
	Node_t *sibling;
	PixB_t  suffix, type;
	union { Node_t *s, **m; } child;
};

typedef struct Gif_CodeTable {
	Node_t nodes[WRITE_CODE_MAX],
	     * links[WRITE_CODE_MAX];
	int links_pos, nodes_pos, clear_code;
} Gif_CodeTable;

typedef struct Gif_Writer {

	FILE *file;

	unsigned char *data;
	unsigned int length, cap;

	unsigned global_size, local_size;
	bool is_cleared;

	short errors;

	Gif_CompressInfo gcinfo;

	void (*Write_byte )(struct Gif_Writer*, const unsigned char );
	void (*Write_chunk)(struct Gif_Writer*, const unsigned char*, const unsigned );
} Gif_Writer;

#define writeChar(c, grr)     (*grr->Write_byte )(grr, c)
#define writeUint8(u, grr)    (*grr->Write_byte )(grr, (unsigned char)(u))
#define writeUint16(u, grr)   (*grr->Write_byte )(grr, (u) & 0xFF), (*grr->Write_byte)(grr, (u) >> 8)
#define writeChunk(buf,l,grr) (*grr->Write_chunk)(grr, buf, l)
#define writeString(str,l,grr)(*grr->Write_chunk)(grr, (const unsigned char *)(str), l)

static void
write_file_byte(Gif_Writer *gwr, const unsigned char b)
{
	int e = fputc(b, gwr->file);
	gwr->errors += (e == EOF);
	gwr->length += (b == e);
}

static void
write_file_chunk(Gif_Writer *gwr, const unsigned char *chunk, const unsigned len)
{
	unsigned n = fwrite(chunk, 1, len, gwr->file);
	gwr->errors += (n != len);
	gwr->length += (n);
}

static void
write_data_byte(Gif_Writer *grr, const unsigned char c)
{
	if (grr->length >= grr->cap) {
		grr->cap = (grr->cap ? grr->cap * 2 : 1024);
		Gif_ReArray(grr->data, unsigned char, grr->cap);
	}
	if (grr->data)
		grr->data[grr->length++] = c;
}

static void
write_data_chunk(Gif_Writer *grr, const unsigned char *chunk, const unsigned len)
{
	while (grr->length + len >= grr->cap) {
		grr->cap = (grr->cap ? grr->cap * 2 : 1024);
		Gif_ReArray(grr->data, unsigned char, grr->cap);
	}
	if (grr->data) {
		memcpy(&grr->data[grr->length], chunk, len);
		grr->length += len;
	}
}


static void
init_writer(Gif_Writer *gwr, FILE *f, const Gif_CompressInfo *gcinfo)
{
	gwr->file = f;
	gwr->data = NULL;

	gwr->length = gwr->cap = gwr->errors = 0;
	gwr->is_cleared = false;

	if (gcinfo)
		gwr->gcinfo = *gcinfo;
	else
		Gif_InitCompressInfo(&gwr->gcinfo);

	gwr->Write_byte  = f ? write_file_byte  : write_data_byte;
	gwr->Write_chunk = f ? write_file_chunk : write_data_chunk;
}

static void
clear_code_table(Gif_CodeTable *c_tab, const Code_t clear_code)
{
	/* The first clear_code nodes are reserved for single-pixel codes */
	c_tab->nodes_pos = c_tab->clear_code = clear_code;
	c_tab->links_pos = 0;
	for (Code_t c = 0; c < clear_code; c++) {
		c_tab->nodes[c].code    = c;
		c_tab->nodes[c].type    = LINKS_TYPE;
		c_tab->nodes[c].suffix  = c;
		c_tab->nodes[c].child.s = NULL;
	}
}

static inline Node_t *
gfc_lookup(Gif_CodeTable *c_tab, Node_t *node, unsigned char suffix)
{
	assert(!node || (node >= c_tab->nodes && node < c_tab->nodes + WRITE_CODE_MAX));
	assert(suffix < c_tab->clear_code);
	if (!node)
		return &c_tab->nodes[suffix];
	else if (node->type == TABLE_TYPE)
		return node->child.m[suffix];
	else {
		for (node = node->child.s; node; node = node->sibling)
			if (node->suffix == suffix)
				return node;
		return NULL;
	}
}

/* Used to hold accumulated error for the current candidate match */
typedef struct gfc_rgbdiff {signed short r, g, b;} gfc_rgbdiff;

/* Difference (MSE) between given color indexes + dithering error */
static inline unsigned int color_diff(Gif_Color aCol, Gif_Color bCol, int a_transparent, int b_transparent, gfc_rgbdiff dither)
{
	unsigned int dith, undith;

	/* if one is transparent and the other is not, then return maximum difference */
	/* TODO: figure out what color is in the canvas under the transparent pixel and match against that */
	if (a_transparent != b_transparent) return 1<<25;

	/* Two transparent colors are identical */
	if (a_transparent) return 0;

	/* squared error with or without dithering. */
	dith = (aCol.R - bCol.R + dither.r) * (aCol.R - bCol.R + dither.r)
	     + (aCol.G - bCol.G + dither.g) * (aCol.G - bCol.G + dither.g)
	     + (aCol.B - bCol.B + dither.b) * (aCol.B - bCol.B + dither.b);

	undith = (aCol.R - bCol.R + dither.r / 2) * (aCol.R - bCol.R + dither.r / 2)
	       + (aCol.G - bCol.G + dither.g / 2) * (aCol.G - bCol.G + dither.g / 2)
	       + (aCol.B - bCol.B + dither.b / 2) * (aCol.B - bCol.B + dither.b / 2);

	/* Smaller error always wins, under assumption that dithering is not required and it's only done opportunistically */
	return dith < undith ? dith : undith;
}

/* difference between expected color a+dither and color b (used to calculate dithering required) */
static inline gfc_rgbdiff diffused_difference(Gif_Color aCol, Gif_Color bCol, int a_transparent, int b_transparent, gfc_rgbdiff dither)
{
	gfc_rgbdiff d;
	if (a_transparent || b_transparent) {
		d.r = d.g = d.b = 0;
	} else {
		d.r = aCol.R - bCol.R + dither.r * 3 / 4;
		d.g = aCol.G - bCol.G + dither.g * 3 / 4;
		d.b = aCol.B - bCol.B + dither.b * 3 / 4;
	}
	return d;
}

static inline unsigned char gif_pixel_at_pos(Gif_Image *gfi, unsigned pos);

static void
gfc_change_node_to_table(Gif_CodeTable *c_tab, Node_t *work_node, Node_t *next_node)
{
	/* change links node to table node */
	Code_t c;
	Node_t **table = &c_tab->links[c_tab->links_pos];
	Node_t *n;
	c_tab->links_pos += c_tab->clear_code;

	for (c = 0; c < c_tab->clear_code; c++)
		table[c] = 0;
	table[next_node->suffix] = next_node;
	for (n = work_node->child.s; n; n = n->sibling)
		table[n->suffix] = n;

	work_node->type = TABLE_TYPE;
	work_node->child.m = table;
}

static inline void
define_code_table(Gif_CodeTable *c_tab, Node_t *work_node, PixB_t suffix, Code_t next_code)
{
	/* Add a new code to our dictionary. First reserve a node for the
		added code. It's LINKS_TYPE at first. */
	Node_t *next_node = &c_tab->nodes[c_tab->nodes_pos++];
	next_node->code = next_code;
	next_node->type = LINKS_TYPE;
	next_node->suffix = suffix;
	next_node->child.s = NULL;

	/* link next_node into work_node's set of children */
	if (work_node->type == TABLE_TYPE)
		work_node->child.m[suffix] = next_node;
	else if (work_node->type < MAX_LINKS_TYPE
			|| c_tab->links_pos + c_tab->clear_code > WRITE_CODE_MAX) {
		next_node->sibling = work_node->child.s;
		work_node->child.s = next_node;
		if (work_node->type < MAX_LINKS_TYPE)
			work_node->type++;
	} else
		gfc_change_node_to_table(c_tab, work_node, next_node);
}

static inline const unsigned char *
gif_imageline(Gif_Image *gfi, unsigned pos)
{
	unsigned y, x;
	if (gfi->width == 0)
		return NULL;
	y = pos / gfi->width;
	x = pos - y * gfi->width;
	if (y == (unsigned) gfi->height)
		return NULL;
	else if (!gfi->interlace)
		return gfi->img[y] + x;
	else
		return gfi->img[Gif_InterlaceLine(y, gfi->height)] + x;
}

static inline unsigned
gif_line_endpos(Gif_Image *gfi, unsigned pos)
{
	unsigned y = pos / gfi->width;
	return (y + 1) * gfi->width;
}

struct selected_node {
	Node_t *node; /* which node has been chosen by gfc_lookup_lossy */
	unsigned long pos, /* where the node ends */
	diff; /* what is the overall quality loss for that node */
};

static inline void
gfc_lookup_lossy_try_node(Gif_CodeTable *gfc, const Gif_Colormap *gfcm, Gif_Image *gfi,
  unsigned pos, Node_t *node, PixB_t suffix, PixB_t next_suffix,
  gfc_rgbdiff dither, unsigned long base_diff, const unsigned int max_diff, struct selected_node *best_t);

/* Recursive loop
 * Find node that is descendant of node (or start new search if work_node is null) that best matches pixels starting at pos
 * base_diff and dither are distortion from search made so far */
static struct selected_node
gfc_lookup_lossy(Gif_CodeTable *gfc, const Gif_Colormap *gfcm, Gif_Image *gfi,
  unsigned pos, Node_t *node, unsigned long base_diff, gfc_rgbdiff dither, const unsigned int max_diff)
{
	const unsigned image_endpos = gfi->width * gfi->height;

	struct selected_node best_t = {node, pos, base_diff};
	PixB_t suffix;
	if (pos >= image_endpos) return best_t;

	suffix = gif_pixel_at_pos(gfi, pos);
	assert(!node || (node >= gfc->nodes && node < gfc->nodes + WRITE_CODE_MAX));
	assert(suffix < gfc->clear_code);
	if (!node) {
		gfc_rgbdiff zero_diff = {0, 0, 0};
		/* prefix of the new node must be same as suffix of previously added node */
		return gfc_lookup_lossy(gfc, gfcm, gfi, pos+1, &gfc->nodes[suffix], base_diff, zero_diff, max_diff);
	}

	/* search all nodes that are less than max_diff different from the desired pixel */
	if (node->type == TABLE_TYPE) {
		int i;
		for(i=0; i < gfc->clear_code; i++) {
			if (!node->child.m[i]) continue;
				gfc_lookup_lossy_try_node(gfc, gfcm, gfi, pos, node->child.m[i], suffix, i, dither, base_diff, max_diff, &best_t);
		}
	} else {
		for (node = node->child.s; node; node = node->sibling) {
			gfc_lookup_lossy_try_node(gfc, gfcm, gfi, pos, node, suffix, node->suffix, dither, base_diff, max_diff, &best_t);
		}
	}

	return best_t;
}

/**
 * Replaces best_t with a new node if it's better
 *
 * @param node        Current node to search
 * @param suffix      Previous pixel
 * @param next_suffix Next pixel to evaluate (must correspond to the node given)
 * @param dither      Desired dithering
 * @param base_diff   Difference accumulated in the search so far
 * @param max_diff    Maximum allowed pixel difference
 * @param best_t      Current best candidate (input/output argument)
 */
static inline void
gfc_lookup_lossy_try_node(Gif_CodeTable *gfc, const Gif_Colormap *gfcm, Gif_Image *gfi,
  unsigned pos, Node_t *node, PixB_t suffix, PixB_t next_suffix,
  gfc_rgbdiff dither, unsigned long base_diff, const unsigned int max_diff, struct selected_node *best_t)
{
	unsigned diff = 0;
	if (suffix != next_suffix)
		diff = color_diff(gfcm->col[suffix], gfcm->col[next_suffix], suffix == gfi->transparent, next_suffix == gfi->transparent, dither);
	if (diff <= max_diff) {
		gfc_rgbdiff new_dither = diffused_difference(gfcm->col[suffix], gfcm->col[next_suffix], suffix == gfi->transparent, next_suffix == gfi->transparent, dither);
		/* if the candidate pixel is good enough, check all possible continuations of that dictionary string */
		struct selected_node t = gfc_lookup_lossy(gfc, gfcm, gfi, pos+1, node, base_diff + diff, new_dither, max_diff);

		/* search is biased towards finding longest candidate that is below treshold rather than a match with minimum average error */
		if (t.pos > best_t->pos || (t.pos == best_t->pos && t.diff < best_t->diff)) {
			*best_t = t;
		}
	}
}

static inline unsigned char
gif_pixel_at_pos(Gif_Image *gfi, unsigned pos)
{
	unsigned y = pos / gfi->width,
	         x = pos - y * gfi->width;
	if (gfi->interlace)
		y = Gif_InterlaceLine(y, gfi->height);
	return gfi->img[y][x];
}

static int
write_compressed_data(Gif_Stream *gfs, Gif_Image *gfi,
                      int min_code_bits, Gif_Writer *grr)
{
	unsigned char stack_buffer[512 - 24];
	unsigned char *buf = stack_buffer;
	unsigned bufpos = 0;
	unsigned bufcap = sizeof(stack_buffer) * 8;

	unsigned pos;
	unsigned clear_bufpos, clear_pos, line_endpos;
	const unsigned image_endpos = gfi->height * gfi->width;
	const unsigned char *imageline;
	const bool has_eager = grr->gcinfo.flags & GIF_WRITE_EAGER_CLEAR;
	unsigned run = 0, run_ewma;

	Node_t *work_node, *next_node;
	Code_t  next_code = 0, output_code;
	PixB_t suffix;

	Gif_CodeTable c_tab = {
		.links_pos = 0, .nodes_pos = 0, .clear_code = 0
	};

	Gif_Colormap *gfcm = gfi->local ? gfi->local : gfs->global;

	int cur_code_bits;

	/* Here we go! */
	writeUint8(min_code_bits, grr);
#define CLEAR_CODE      ((Code_t) (1 << min_code_bits))
#define EOI_CODE        ((Code_t) (CLEAR_CODE + 1))
#define CUR_BUMP_CODE   (1 << cur_code_bits)
	grr->is_cleared = false;

	cur_code_bits = min_code_bits + 1;
	/* next_code set by first runthrough of output clear_code */
	GIF_DEBUG(("clear(%d) eoi(%d) bits(%d) ", CLEAR_CODE, EOI_CODE, cur_code_bits));

	work_node = NULL;
	output_code = CLEAR_CODE;
	/* Because output_code is clear_code, we'll initialize next_code, et al. below. */

	pos = clear_pos = clear_bufpos = 0;
	line_endpos = gfi->width;
	imageline = gif_imageline(gfi, pos);

#define EWMA_PAD_B 19
#define _RUN_ENWA_ \
		run = (run << EWMA_PAD_B) + (1 << (0x04 - 1));\
	if (run < run_ewma)\
		run_ewma -= (run_ewma - run) >> 0x04;\
	else\
		run_ewma += (run - run_ewma) >> 0x04;

#define _NEXT_CODE_ \
	if (next_code < WRITE_CODE_MAX)\
		define_code_table(&c_tab, work_node, suffix, next_code++);\
	else\
		next_code = WRITE_CODE_MAX + 1; // to match `i > CUR_BUMP_CODE` above

/* Always clear if run_ewma gets small relative to
	min_code_bits. Otherwise, clear if #images/run is smaller
	than an empirical threshold, meaning it will take more than
	3000 or so average runs to complete the image. */
#define _DO_CLEAR(_0_,_1_) (\
	has_eager ?: (\
		run_ewma < ((36U << EWMA_PAD_B) / min_code_bits) ||\
		           (image_endpos - pos - _0_) > UINT32_MAX / ((unsigned)(1 << EWMA_PAD_B) / 3000) ||\
		run_ewma < (image_endpos - pos - _0_) *              ((unsigned)(1 << EWMA_PAD_B) / 3000)\
	));\
	if ((do_clear || run < 7) && !clear_pos) {\
		clear_pos = pos - (run + _1_);\
		clear_bufpos = bufpos;\
	} else if (!do_clear && run > 50) {\
		clear_pos = clear_bufpos = 0;\
	}

  while (1) {

	/* Output 'output_code' to the memory buffer.*/
	if (bufpos + 32 >= bufcap) {
		unsigned ncap = bufcap * 2 + (24 << 3);
		unsigned char *nbuf = Gif_NewArray(unsigned char, ncap >> 3);
		if (!nbuf)
			goto error;
		memcpy(nbuf, buf, bufcap >> 3);
		if (buf != stack_buffer)
			Gif_DeleteArray(buf);
		buf = nbuf;
		bufcap = ncap;
	}
	unsigned endpos = bufpos + cur_code_bits;

	do {
		if (bufpos & 7)
			buf[bufpos >> 3] |= output_code << (bufpos & 7);
		else if (bufpos & 0x7FF)
			buf[bufpos >> 3]  = output_code >> (bufpos - endpos + cur_code_bits);
		else {
			buf[bufpos >> 3]  = 255;
			endpos += 8;
		}
		bufpos += 8 - (bufpos & 7);
	} while (bufpos < endpos);

	bufpos = endpos;

	/* Handle special codes. */
	if (output_code == CLEAR_CODE) {
		/* Clear data and prepare gfc */
		cur_code_bits = min_code_bits + 1;
		next_code     = EOI_CODE + 1;
		run_ewma      = 1 << EWMA_PAD_B;
		clear_bufpos  = clear_pos = run = 0;
		clear_code_table(&c_tab, CLEAR_CODE);
	} else if (output_code == EOI_CODE) {
		break;

	} else {
		if (next_code > CUR_BUMP_CODE && cur_code_bits < WRITE_CODE_BITS)
			/* bump up compression size */
			cur_code_bits++;
		/* Adjust current run length average. */
		_RUN_ENWA_;
		/* Reset run length. */
		run = !!work_node;
	}

	/* Find the next code to output. */
	if (grr->gcinfo.lossy) {
		gfc_rgbdiff zero_diff = {0, 0, 0};
		struct selected_node t = gfc_lookup_lossy(&c_tab, gfcm, gfi, pos, NULL, 0, zero_diff, grr->gcinfo.lossy * 10);

		work_node = t.node;
		run = t.pos - pos;
		pos = t.pos;

		if (pos < image_endpos) {
			suffix = gif_pixel_at_pos(gfi, pos);
			/* Output the current code. */
			_NEXT_CODE_;
			/* Check whether to clear table. */
			if (next_code > 4094) {
				bool do_clear = _DO_CLEAR(1,0);

				if (do_clear) {
					GIF_DEBUG(("rewind %u pixels/%d bits", pos + 1 - clear_pos, bufpos + cur_code_bits - clear_bufpos));
					output_code = CLEAR_CODE;
					pos = clear_pos;
					bufpos = clear_bufpos;
					buf[bufpos >> 3] &= (1 << (bufpos & 7)) - 1;
					grr->is_cleared = true;
					continue;
				}
			}
			/* Adjust current run length average. */
			_RUN_ENWA_;
		}
		output_code = (work_node ? work_node->code : EOI_CODE);
	} else {
	  /* If height is 0 -- no more pixels to write -- we output work_node next
	     time around. */
		while (imageline) {
			suffix = *imageline;
			next_node = gfc_lookup(&c_tab, work_node, suffix);
			imageline++;
			if (++pos == line_endpos) {
				imageline = gif_imageline(gfi, pos);
				line_endpos += gfi->width;
			}
			if (next_node) {
				work_node = next_node;
				run++;
				continue;
			}
			/* Output the current code. */
			_NEXT_CODE_;
			/* Check whether to clear table. */
			if (next_code > 4094) {
				bool do_clear = _DO_CLEAR(0,1);

				if (do_clear) {
					GIF_DEBUG(("rewind %u pixels/%d bits ", pos - clear_pos, bufpos + cur_code_bits - clear_bufpos));
					output_code = CLEAR_CODE;
					pos = clear_pos;
					imageline = gif_imageline(gfi, pos);
					line_endpos = gif_line_endpos(gfi, pos);
					bufpos = clear_bufpos;
					buf[bufpos >> 3] &= (1 << (bufpos & 7)) - 1;
					work_node = NULL;
					grr->is_cleared = true;
					goto found_output_code;
				}
			}
			output_code = work_node->code;
			work_node = &c_tab.nodes[suffix];
			goto found_output_code;
		}
		/* Ran out of data if we get here. */
		output_code = (work_node ? work_node->code : EOI_CODE);
		work_node = NULL;

		found_output_code: ;
	}
  }

	/* Output memory buffer to stream. */
	bufpos = (bufpos + 7) >> 3;
	buf[(bufpos - 1) & 0xFFFFFF00] = (bufpos - 1) & 0xFF;
	buf[bufpos] = 0;
	writeChunk(buf, bufpos + 1, grr);

	if (buf != stack_buffer)
		Gif_DeleteArray(buf);
	return 1;

error:
	if (buf != stack_buffer)
		Gif_DeleteArray(buf);
	return 0;
}


static int
calculate_min_code_bits(Gif_Image *gfi, const Gif_Writer *grr)
{
	int colors_used = 0, min_code_bits = 2, x, y;

	if (grr->gcinfo.flags & GIF_WRITE_CAREFUL_MIN_CODE_SIZE) {
		/* calculate m_c_b based on colormap */
		colors_used = grr->local_size ?: grr->global_size ?: -1;
	} else if (gfi->img) {
		/* calculate m_c_b from uncompressed data */
		for (y = 0; y < gfi->height && colors_used < 128; y++) {
			for (x = 0; x < gfi->width; x++) {
				if (gfi->img[y][x] > colors_used)
					colors_used = gfi->img[y][x];
			}
		}
		colors_used += 1;
	} else if (gfi->compressed) {
		/* take m_c_b from compressed image */
		colors_used = 1 << gfi->compressed[0];

	} else {
		/* should never happen */
		colors_used = 256;
	}
	/* min_code_bits of 1 isn't allowed */
	for (y = 4; y < colors_used; y *= 2)
		min_code_bits++;

	return min_code_bits;
}


static unsigned get_color_table_size(const Gif_Stream *gfs, Gif_Image *gfi,
                                Gif_Writer *grr);

static void
save_compression_result(Gif_Image *gfi, Gif_Writer *grr, int ok)
{
	if (!(grr->gcinfo.flags & GIF_WRITE_SHRINK)
		|| (ok && (!gfi->compressed || gfi->compressed_len > grr->length))) {
		if (gfi->compressed)
			Gif_Delete(gfi->compressed);
		if (ok) {
			gfi->compressed_len = grr->length;
			gfi->compressed_errors = 0;
			gfi->compressed = grr->data;
			grr->data = NULL;
			grr->cap = 0;
		}
	}
	grr->length = 0;
}

int
Gif_FullCompressImage(Gif_Stream *gfs, Gif_Image *gfi,
                      const Gif_CompressInfo *gcinfo)
{
	bool ok = false;
	unsigned char min_code_bits;
	Gif_Writer grr;

	init_writer(&grr, NULL, gcinfo);

	if (!(grr.gcinfo.flags & GIF_WRITE_SHRINK))
		Gif_ReleaseCompressedImage(gfi);
	
	grr.global_size = get_color_table_size(gfs, NULL, &grr);
	grr.local_size  = get_color_table_size(gfs, gfi, &grr);

	min_code_bits = calculate_min_code_bits(gfi, &grr);
	ok = write_compressed_data(gfs, gfi, min_code_bits, &grr);
	save_compression_result(gfi, &grr, ok);

	if ((grr.gcinfo.flags & (GIF_WRITE_OPTIMIZE | GIF_WRITE_EAGER_CLEAR))
		== GIF_WRITE_OPTIMIZE
		&& grr.is_cleared && ok) {
		grr.gcinfo.flags |= GIF_WRITE_EAGER_CLEAR | GIF_WRITE_SHRINK;
		if (write_compressed_data(gfs, gfi, min_code_bits, &grr))
			save_compression_result(gfi, &grr, 1);
	}
	return ok;
}


static unsigned
get_color_table_size(const Gif_Stream *gfs, Gif_Image *gfi, Gif_Writer *grr)
{
	Gif_Colormap *gfcm = (gfi ? gfi->local : gfs->global);
	int ncol, totalcol, i;

	if (!gfcm || gfcm->ncol <= 0)
		return 0;

	/* Make sure ncol is reasonable */
	ncol = gfcm->ncol;

	/* Possibly bump up 'ncol' based on 'transparent' values, if
		careful_min_code_bits */
	if (grr->gcinfo.flags & GIF_WRITE_CAREFUL_MIN_CODE_SIZE) {
		if (gfi && gfi->transparent >= ncol)
			ncol = gfi->transparent + 1;
		else if (!gfi)
			for (i = 0; i < gfs->nimages; i++)
				if (gfs->images[i]->transparent >= ncol)
					ncol = gfs->images[i]->transparent + 1;
	}

	/* Make sure the colormap is a power of two entries!
	   GIF format doesn't allow a colormap with only 1 entry. */
	if (ncol > 256)
		ncol = 256;
	for (totalcol = 2; totalcol < ncol; totalcol *= 2)
		/* nada */;

	return totalcol;
}

static void
write_color_table(Gif_Colormap *gfcm, int totalcol, Gif_Writer *grr)
{
	Gif_Color *c = gfcm->col;
	int i, limit = _MIN(gfcm->ncol, totalcol);

	for (i = 0; i < limit; i++) {
		writeUint8(c[i].R, grr);
		writeUint8(c[i].G, grr);
		writeUint8(c[i].B, grr);
	}
	/* Pad out colormap with black. */
	for (; i < totalcol; i++) {
		writeUint8(0, grr);
		writeUint8(0, grr);
		writeUint8(0, grr);
	}
}


static bool
write_image(Gif_Stream *gfs, Gif_Image *gfi, Gif_Writer *grr)
{
	unsigned char min_code_bits, packed = 0;
	grr->local_size = get_color_table_size(gfs, gfi, grr);

	writeChar  (','        , grr);
	writeUint16(gfi->left  , grr);
	writeUint16(gfi->top   , grr);
	writeUint16(gfi->width , grr);
	writeUint16(gfi->height, grr);

	if (grr->local_size > 0) {
		int size = 2;
		packed |= 0x80;
		while (size < grr->local_size)
			size *= 2, packed++;
	}

	if (gfi->interlace)
		packed |= 0x40;
	writeUint8(packed, grr);

	if (grr->local_size > 0)
		write_color_table(gfi->local, grr->local_size, grr);

	/* calculate min_code_bits here (because calculation may involve
		recompression, if GIF_WRITE_CAREFUL_MIN_CODE_SIZE is true) */
	min_code_bits = calculate_min_code_bits(gfi, grr);

	/* use existing compressed data if it exists. This will tend to whip
		people's asses who uncompress an image, keep the compressed data around,
		but modify the uncompressed data anyway. That sucks. */
	if (gfi->compressed
		&& (!(grr->gcinfo.flags & GIF_WRITE_CAREFUL_MIN_CODE_SIZE)
			|| gfi->compressed[0] == min_code_bits)) {
		unsigned char *compressed = gfi->compressed;
		unsigned compressed_len = gfi->compressed_len;
		while (compressed_len > 0) {
			unsigned short amt = _MIN(compressed_len, 0x7000);
			writeChunk(compressed, amt, grr);
			compressed += amt;
			compressed_len -= amt;
		}

	} else if (!gfi->img) {
		Gif_UncompressImage(gfs, gfi);
		write_compressed_data(gfs, gfi, min_code_bits, grr);
		Gif_ReleaseUncompressedImage(gfi);
	} else
		write_compressed_data(gfs, gfi, min_code_bits, grr);

	return true;
}


static void
write_logical_screen_descriptor(Gif_Stream *gfs, Gif_Writer *grr)
{
	unsigned char packed = 0x70; /* high resolution colors */
	unsigned i, g_size = (grr->global_size = get_color_table_size(gfs, 0, grr));

	Gif_CalcScreenSize(gfs, false);
	writeUint16(gfs->screen_width , grr);
	writeUint16(gfs->screen_height, grr);

	if (g_size > 0) {
		packed |= 0x80;
		for (i = 2; i < g_size; i *= 2)
			packed++;
	}
	writeUint8(packed, grr);
	writeUint8(gfs->background < g_size ? gfs->background : 255, grr);
	writeUint8(0, grr);/* no aspect ratio information */

	if (g_size > 0)
		write_color_table(gfs->global, g_size, grr);
}


/* extension byte table:
   0x01 plain text extension
   0xCE name*
   0xF9 graphic control extension
   0xFE comment extension
   0xFF application extension
   */

static void
write_graphic_control_extension(Gif_Image *gfi, Gif_Writer *grr)
{
	unsigned char packed = 0;
	short transp = gfi->transparent;
	writeChar  ('!' , grr);
	writeUint8 (0xF9, grr);
	writeUint8 (0x04, grr);
	if (transp >= 0)
		packed |= 0x01;
	packed |=  (gfi->disposal & 0x07) << 2;
	writeUint8 (packed    , grr);
	writeUint16(gfi->delay, grr);
	writeUint8 (transp    , grr);
	writeChar  ('\0'      , grr);
}

#define writeData(_W_,_D_,_N_) {\
	unsigned pos = 0, len = _N_;\
\
	while (len > 0) {\
		unsigned char b = _MIN(len, WRITE_BUFFER_SIZE);\
		writeUint8(b, _W_);\
		writeChunk(&_D_[pos], b, _W_);\
		pos += b, len -= b;\
	}\
}

static void
write_name_extension(Gif_Writer *gwr, const char *name)
{
	writeChar('!', gwr);
	writeUint8(0xCE, gwr);
	writeData(gwr, name, strlen(name));
	writeChar('\0', gwr);
}

static void
write_comment_extensions(Gif_Writer *gwr, Gif_Comment *gcom)
{
	for (int i = 0; i < gcom->indents; i++) {
		writeChar('!', gwr);
		writeUint8(0xFE, gwr);
		writeData(gwr, gcom->str[i], gcom->len[i]);
		writeChar('\0', gwr);
	}
}


static void
write_netscape_loop_extension(unsigned short value, Gif_Writer *grr)
{
	writeString("!\xFF\x0BNETSCAPE2.0\x03\x01", 16, grr);
	writeUint16(value, grr);
	writeChar('\0', grr);
}


static void
write_generic_extension(Gif_Writer *gwr, Gif_Extension *gfex)
{
	for (;  gfex; gfex = gfex->next) {

		if (gfex->kind < 0)
			continue; /* ignore our private extensions */

		writeChar('!', gwr);
		writeUint8(gfex->kind, gwr);
		if (gfex->kind == 255) { /* an application extension */
			if (gfex->applength) {
				writeUint8(gfex->applength, gwr);
				writeString(gfex->appname, gfex->applength, gwr);
			}
		}
		if (gfex->packetized)
			writeChunk(gfex->data, gfex->length, gwr);
		else
			writeData(gwr, gfex->data, gfex->length);
		writeChar('\0', gwr);
	}
}

static bool
incremental_write_image(Gif_Writer* gwr, Gif_Stream* gfs, Gif_Image* gfi)
{
	if (gfi->extension_list)
		write_generic_extension(gwr, gfi->extension_list);
	if (gfi->comment)
		write_comment_extensions(gwr, gfi->comment);
	if (gfi->identifier)
		write_name_extension(gwr, gfi->identifier);
	if (gfi->transparent != -1 || gfi->disposal || gfi->delay)
		write_graphic_control_extension(gfi, gwr);
	return write_image(gfs, gfi, gwr);
}

static bool
write_gif(Gif_Stream *gfs, Gif_Writer *grr)
{
	bool isgif89a = false;

	if (gfs->end_comment || gfs->end_extension_list || gfs->loopcount > -1) {
		isgif89a = true;
	} else {
		for (int i = 0; i < gfs->nimages; i++) {
			Gif_Image* gfi = gfs->images[i];
			if (gfi->identifier || gfi->transparent != -1 || gfi->disposal
				|| gfi->delay || gfi->comment || gfi->extension_list) {
				isgif89a = true;
				break;
			}
		}
	}
	writeString(isgif89a ? "GIF89a" : "GIF87a", 6, grr);

	write_logical_screen_descriptor(gfs, grr);

	if (gfs->loopcount > -1)
		write_netscape_loop_extension(gfs->loopcount, grr);

	for (int i = 0; i < gfs->nimages; i++)
		if (!incremental_write_image(grr, gfs, gfs->images[i]))
			return false;

	if (gfs->end_extension_list)
		write_generic_extension(grr, gfs->end_extension_list);
	if (gfs->end_comment)
		write_comment_extensions(grr, gfs->end_comment);

	writeChar(';', grr);
	return true;
}

/*
  interface functions 
  returns => size of writing file/data
*/
unsigned int Gif_FullWriteFile(
	Gif_Stream       *gst,
	Gif_CompressInfo *gcinfo,
	FILE             *file
) {
	Gif_Writer gwr;

	init_writer(&gwr, file, gcinfo);

	if (!write_gif(gst, &gwr)) {
		/* check errors */;
	}
	return gwr.length;
}

unsigned int Gif_FullWriteData(
	Gif_Stream       *gst,
	Gif_CompressInfo *gcinfo,
	unsigned char   **out
) {
	Gif_Writer gwr;

	init_writer(&gwr, NULL, gcinfo);

	if (!write_gif(gst, &gwr)) {
		/* check errors */;
	}
	*out = gwr.data, gwr.data = NULL;
	return gwr.length;
}
