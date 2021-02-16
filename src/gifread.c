/* gifread.c - Functions to read GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of the LCDF GIF library.

   The LCDF GIF library is free software. It is distributed under the GNU
   General Public License, version 2; you can copy, distribute, or alter it at
   will, as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied. */

#include <gifsi.h>

typedef struct {

	Gif_Stream     *stream;
	Gif_Image      *img;

	Gif_Code       *prefix;
	unsigned char  *suffix;
	unsigned short *length;

	unsigned short width, height;
	unsigned char *image, *maximage;
	unsigned int decodepos;

	Gif_ReadErrorHandler handler;
	int errors[2];

} Gif_Context;

static bool make_context(Gif_Context *gctx, Gif_ReadErrorHandler handler)
{
	gctx->errors[0] = gctx->errors[1] = 0;
	gctx->handler   = handler;
	return (
		(gctx->prefix = Gif_NewArray(Gif_Code      , GIF_MAX_CODE)) &&
		(gctx->suffix = Gif_NewArray(unsigned char , GIF_MAX_CODE)) &&
		(gctx->length = Gif_NewArray(unsigned short, GIF_MAX_CODE))
	);
}
static void clear_context(Gif_Context *gctx)
{
	Gif_DeleteArray(gctx->prefix);
	Gif_DeleteArray(gctx->suffix);
	Gif_DeleteArray(gctx->length);
}

typedef struct Gif_Reader {

	FILE *file;

	const unsigned char *data;
	unsigned pos, length;

	bool is_record, is_end;

	unsigned char (*Read_byte )(struct Gif_Reader *);
	unsigned      (*Read_chunk)(struct Gif_Reader *, unsigned char *, unsigned);
	void          (*Skip_bytes)(struct Gif_Reader *, unsigned );
} Gif_Reader;

static Gif_ReadErrorHandler default_error_handler = NULL;

#define readChar(grr)   ((char)(*grr->Read_byte )(grr))
#define readUint8(grr)        ((*grr->Read_byte )(grr))
#define readUint16(grr)       ((*grr->Read_byte )(grr) | (*grr->Read_byte)(grr) << 8)
#define skipBytes(grr,off)    ((*grr->Skip_bytes)(grr, off))
#define readChunk(grr,buf,sz) ((*grr->Read_chunk)(grr, buf, sz))

#define read_compressed_image(r,i,f) (r->is_record ? \
	read_compressed_image_data(r,i,f) : \
	read_compressed_image_file(r,i,f))

static void
skip_file_bytes(Gif_Reader *grr, unsigned offset)
{
	fseek(grr->file, offset, SEEK_CUR);
	unsigned diff = ftell(grr->file) - grr->pos;
	grr->is_end = feof(grr->file) != 0;
	grr->pos += diff != offset ? diff : offset;
}

static unsigned char
read_file_byte(Gif_Reader *grr)
{
	int c = getc(grr->file);
	if (c == EOF) {
		grr->is_end = true;
		return 0;
	}
	grr->pos++;
	return c;
}

static unsigned
read_file_chunk(Gif_Reader *grr, unsigned char *buf, unsigned size)
{
	unsigned chunk = fread(buf, 1, size, grr->file);
	if (chunk < size) {
		memset(buf + chunk, 0, size - chunk);
		grr->is_end = (bool)feof(grr->file);
	}
	grr->pos += chunk;
	return chunk;
}

static void
make_file_reader(Gif_Reader *grr, FILE *file)
{
	grr->file       = file;
	grr->pos        = 0;
	grr->is_end     = false;
	grr->is_record  = false;
	grr->Read_byte  = read_file_byte;
	grr->Read_chunk = read_file_chunk;
	grr->Skip_bytes = skip_file_bytes;
}

static unsigned char
read_data_byte(Gif_Reader *grr) {
	return (grr->is_end = grr->pos >= grr->length) ? 0 : grr->data[grr->pos++];
}

static unsigned
read_data_chunk(Gif_Reader *grr, unsigned char *buf, unsigned size)
{
	unsigned chunk = (grr->pos + size <= grr->length ? size : grr->length - grr->pos);
	memcpy(buf, &grr->data[grr->pos], chunk);
	grr->is_end = (grr->pos += chunk) >= grr->length;
	if (chunk < size)
		memset(buf + chunk, 0, size - chunk);
	return chunk;
}

static void
skip_data_bytes(Gif_Reader *grr, unsigned offset)
{
	unsigned new_pos = grr->pos + offset;
	grr->pos = (grr->is_end = new_pos >= grr->length) ? grr->length : new_pos;
}

static void
make_data_reader(Gif_Reader *grr, const unsigned char *data, unsigned length)
{
	grr->data       = data;
	grr->pos        = 0;
	grr->length     = length;
	grr->is_end     = false;
	grr->is_record  = true;
	grr->Read_byte  = read_data_byte;
	grr->Read_chunk = read_data_chunk;
	grr->Skip_bytes = skip_data_bytes;
}

static void
emit_read_error(Gif_Context *gctx, int error_flag, const char *text)
{
	Gif_ReadErrorHandler handler = gctx->handler ? gctx->handler : default_error_handler;
	if (error_flag >= 0)
		gctx->errors[error_flag > 0] += 1;
	if (handler)
		handler(gctx->stream, gctx->img, error_flag, text);
}

static unsigned char
one_code(Gif_Context *gctx, Gif_Code code)
{
	int lastsuffix = 0;
	int codelength = gctx->length[code];

	unsigned char *suffx = gctx->suffix;
	unsigned char *ptr   = gctx->image + (gctx->decodepos += codelength);
	Gif_Code      *prefx = gctx->prefix;

	while ((codelength--) > 0) {
		lastsuffix = suffx[code];
		code       = prefx[code];
		if (--ptr < gctx->maximage)
			 *ptr = lastsuffix;
	}
	/* return the first pixel in the code, which, since we walked backwards
	   through the code, was the last suffix we processed. */
	return lastsuffix;
}

static bool
read_image_block(Gif_Reader *grr, unsigned char *buffer, int *bit_pos_store,
                 int *bit_len_store, int bits_needed)
{
	int bit_position = *bit_pos_store;
	int bit_length   = *bit_len_store;
	unsigned char block_len;

	while (bit_position + bits_needed > bit_length) {
		/* Read in the next data block. */
		if (bit_position >= 8) {
			/* Need to shift down the upper, unused part of 'buffer' */
			int i = bit_position / 8;
			buffer[0] = buffer[i];
			buffer[1] = buffer[i + 1];
			bit_position -= i * 8;
			bit_length   -= i * 8;
		}
		block_len = readUint8(grr);
		GIF_DEBUG(("\nimage_block(%d) ", block_len));
		if (!block_len) return false;
		readChunk(grr, buffer + bit_length / 8, block_len);
		bit_length += block_len * 8;
	}
	*bit_pos_store = bit_position;
	*bit_len_store = bit_length;
	return true;
}

static void
read_image_data(Gif_Context *gctx, Gif_Reader *grr)
{
	/* we need a bit more than GIF_MAX_BLOCK in case a single code is split
		across blocks */
	unsigned char buffer[GIF_MAX_BLOCK + 5];
	unsigned int  accum;
	int i;
	
	int bits_needed  = 0,
	    bit_position = 0,
	    bit_length   = 0;

	Gif_Code code, old_code, next_code, eoi_code, clear_code;

#define CUR_BUMP_CODE ( 1 << bits_needed)
#define CUR_CODE_MASK ((1 << bits_needed) - 1)

	int min_code_size = readUint8(grr);

	GIF_DEBUG(("\n\nmin_code_size(%d) ", min_code_size));

	if (min_code_size >= GIF_MAX_CODE_BITS) {
		emit_read_error(gctx, 1, "image corrupted, min_code_size too big");
		min_code_size = GIF_MAX_CODE_BITS - 1;
	} else if (min_code_size < 2) {
		emit_read_error(gctx, 1, "image corrupted, min_code_size too small");
		min_code_size = 2;
	}
	bits_needed = 1 +  min_code_size;
	clear_code  = 1 << min_code_size;
	/* */ code  = gctx->decodepos = 0;

	for (; code < clear_code; code++) {
		gctx->prefix[code] = 49428;
		gctx->suffix[code] = (unsigned char)code;
		gctx->length[code] = 1;
	}
	next_code = eoi_code = clear_code + 1;

	/* Thus the 'Read in the next data block.' code below will be invoked on the
	   first time through: exactly right! */
	while (1) {

		old_code = code;

	/* GET A CODE INTO THE 'code' VARIABLE.
	*
	* 9.Dec.1998 - Rather than maintain a byte pointer and a bit offset into
	* the current byte (and the processing associated with that), we maintain
	* one number: the offset, in bits, from the beginning of 'buffer'. This
	* much cleaner choice was inspired by Patrick J. Naughton
	* <naughton@wind.sun.com>'s GIF-reading code, which does the same thing.
	* His code distributed as part of XV in xvgif.c. */

		if (bit_position + bits_needed > bit_length)
	/* Read in the next data block. */
		if (!read_image_block(grr, buffer, &bit_position, &bit_length, bits_needed))
			goto zero_length_block;

		i = bit_position / 8;
		accum = buffer[i] + (buffer[i + 1] << 8);
		if (bits_needed >= 8)
			accum |= (buffer[i+2]) << 16;
		code = (Gif_Code)((accum >> (bit_position % 8)) & CUR_CODE_MASK);
		bit_position += bits_needed;

		GIF_DEBUG(("%d ", code));

	/* CHECK FOR SPECIAL OR BAD CODES: clear_code, eoi_code, or a code that is
	* too large. */
		if (code == clear_code) {
			GIF_DEBUG(("clear "));
			bits_needed = min_code_size + 1;
			next_code   = eoi_code;
			continue;

		} else if (code == eoi_code) {
			break;

		} else if (code > next_code && next_code && next_code != clear_code) {
	/* code > next_code: a (hopefully recoverable) error.

	* Bug fix, 5/27: Do this even if old_code == clear_code, and set code
	* to 0 to prevent errors later. (If we didn't zero code, we'd later set
	* old_code = code; then we had old_code >= next_code; so the prefixes
	* array got all screwed up!)

	* Bug fix, 4/12/2010: It is not an error if next_code == clear_code.
	* This happens at the end of a large GIF: see the next comment ("If no
	* meaningful next code should be defined...."). */
			if (gctx->errors[1] < 20)
				emit_read_error(gctx, 1, "image corrupted, code out of range");
			else if (gctx->errors[1] == 20)
				emit_read_error(gctx, 1, "(not reporting more errors)");
			code = 0;
		}
	/* PROCESS THE CURRENT CODE and define the next code. If no meaningful
	* next code should be defined, then we have set next_code to either
	* 'eoi_code' or 'clear_code' -- so we'll store useless prefix/suffix data
	* in a useless place. */

	/* *First,* set up the prefix and length for the next code
	 (in case code == next_code). */
		gctx->prefix[next_code] = old_code;
		gctx->length[next_code] = gctx->length[old_code] + 1;

	/* Use one_code to process code. It's nice that it returns the first
	 pixel in code: that's what we need. */
		gctx->suffix[next_code] = one_code(gctx, code);

	/* Special processing if code == next_code: we didn't know code's final
	* suffix when we called one_code, but we do now.
	* 7.Mar.2014 -- Avoid error if image has zero width/height. */
		if (code == next_code && gctx->image + gctx->decodepos <= gctx->maximage)
			gctx->image[gctx->decodepos - 1] = gctx->suffix[next_code];

	/* Increment next_code except for the 'clear_code' special case (that's
	 when we're reading at the end of a GIF) */
		if (next_code != clear_code) {
			if (++next_code == CUR_BUMP_CODE) {
				if (bits_needed < GIF_MAX_CODE_BITS)
					bits_needed++;
				else
					next_code = clear_code;
			}
		}
	}

	/* read blocks until zero-length reached. */
	while ((i = readUint8(grr)) > 0) {
		readChunk(grr, buffer, i);
		GIF_DEBUG(("\nafter_image(%d)\n", i));
	}

	/* zero-length block reached. */
	zero_length_block: {
		long delta = (long)(gctx->maximage - gctx->image) - (long)gctx->decodepos;
		char buf[BUFSIZ];
		if (delta > 0) {
			sprintf(buf, "missing %ld %s of image data", delta,
					delta == 1 ? "pixel" : "pixels");
			emit_read_error(gctx, 1, buf);
			memset(&gctx->image[gctx->decodepos], 0, delta);
		} else if (delta < -1) {
			/* One pixel of superfluous data is OK; that could be the
				code == next_code case. */
			sprintf(buf, "%ld superfluous pixels of image data", -delta);
			emit_read_error(gctx, 0, buf);
		}
	}
}

static Gif_Colormap *
read_color_table(Gif_Reader *grr, const int ncol)
{
	Gif_Colormap *gfcm = Gif_NewColormap(ncol, ncol);
	if (gfcm != NULL) {
		GIF_DEBUG(("colormap(%d) ", ncol));
		for (int i = 0; i < ncol; i++) {
			gfcm->col[i].R = readUint8(grr);
			gfcm->col[i].G = readUint8(grr);
			gfcm->col[i].B = readUint8(grr);
			gfcm->col[i].haspixel = 0;
		}
	}
	return gfcm;
}

static bool
read_logical_screen_descriptor(Gif_Reader *grr, Gif_Stream *gfs)
{
	/* we don't care about logical screen width or height */
	gfs->screen_width  = readUint16(grr);
	gfs->screen_height = readUint16(grr);

	unsigned char pack = readUint8(grr);
	   gfs->background = readUint8(grr);
	/* don't care about pixel aspect ratio */
	skipBytes(grr, 1);

	if (pack & 0x80) { /* have a global color table */
		int size = 1 << ((pack & 0x07) + 1);
		if (!(gfs->global = read_color_table(grr, size)))
			return false; /* returns false on memory error */
		gfs->global->refcount = 1;
	} else
		gfs->background = 256;
	return true;
}

static bool
read_compressed_image_data(Gif_Reader *grr, Gif_Image *gfi, int flags)
{
	const unsigned image_pos = grr->pos;
	/* scan over image */
	grr->pos++; /* skip min code size */
	while (grr->pos < grr->length) {
		int amt = grr->data[grr->pos];
		grr->pos += amt + 1;
		if (amt == 0)
			break;
	}
	if (grr->pos > grr->length)
		grr->pos = grr->length;
	gfi->compressed_len = grr->pos - image_pos;
	gfi->compressed_errors = 0;
	if (flags & GIF_READ_CONST_RECORD) {
		gfi->compressed = (unsigned char *) &grr->data[image_pos];
		gfi->free_compressed = 0;
	} else {
		gfi->compressed = Gif_NewArray(unsigned char, gfi->compressed_len);
		gfi->free_compressed = Gif_Free;
		if (!gfi->compressed)
			return false;
		memcpy(gfi->compressed, &grr->data[image_pos], gfi->compressed_len);
	}
	return true;
}

static bool
read_compressed_image_file(Gif_Reader *grr, Gif_Image *gfi, int flags)
{
	/* non-record; have to read it block by block. */
	unsigned   comp_cap = 1024, comp_len;
	unsigned char *comp = Gif_NewArray(unsigned char, comp_cap);
	if (!comp)
		return false;

	/* min code size */
	comp[0] = readUint8(grr);
	comp_len = 1;

	for (int i; (i = readUint8(grr)) > 0;) {
		/* add 2 before check so we don't have to check after loop when appending
		   0 block */
		if (comp_len + i + 2 > comp_cap) {
			comp_cap *= 2;
			Gif_ReArray(comp, unsigned char, comp_cap);
			if (!comp)
				return false;
		}
		comp[comp_len] = i;
		readChunk(grr, comp + comp_len + 1, i);
		comp_len += i + 1;
	}
	comp[comp_len]         = 0;
	gfi->compressed_len    = comp_len + 1;
	gfi->compressed_errors = 0;
	gfi->compressed        = comp;
	gfi->free_compressed   = Gif_Free;
	return true;
}

static bool
uncompress_image(Gif_Context *gctx, Gif_Image *gfi, Gif_Reader *grr)
{
	if (!Gif_CreateUncompressedImage(gfi, gfi->interlace))
		return false;
	gctx->width    = gfi->width;
	gctx->height   = gfi->height;
	gctx->image    = gfi->image_data;
	gctx->maximage = gfi->image_data + (unsigned)gfi->width * (unsigned)gfi->height;

	int old_nerrors = gctx->errors[1];
	read_image_data(gctx, grr);
	gfi->compressed_errors = gctx->errors[1] - old_nerrors;
	return true;
}


int
Gif_FullUncompressImage(Gif_Stream* gfs, Gif_Image* gfi,
                        Gif_ReadErrorHandler handler)
{
	Gif_Context gctx;
	Gif_Reader grr;
	int ok = 0;

	/* return right away if image is already uncompressed. this might screw over
		people who expect re-uncompressing to restore the compressed version. */
	if (gfi->img)
		return 2;
	if (gfi->image_data)
		/* we have uncompressed data, but not an 'img' array;
		this shouldn't happen */
		return 0;

	gctx.stream = gfs;
	gctx.img    = gfi;
	
	if (make_context(&gctx, handler) && gfi->compressed) {
		make_data_reader(&grr, gfi->compressed, gfi->compressed_len);
		ok = uncompress_image(&gctx, gfi, &grr);
	}
	clear_context(&gctx);

	if (gctx.errors[0] || gctx.errors[1])
		emit_read_error(&gctx, -1, NULL);
	return ok && !gctx.errors[1];
}

static int
read_image(Gif_Context *gctx, Gif_Reader *grr, Gif_Stream *gfs, Gif_Image *gfi, int flags)
{
	bool     ok     = true;
	unsigned left   = (gfi->left   = readUint16(grr) );
	unsigned top    = (gfi->top    = readUint16(grr) );
	unsigned width  = (gfi->width  = readUint16(grr) ?: gfs->screen_width);
	unsigned height = (gfi->height = readUint16(grr) ?: gfs->screen_height);
  /* Mainline GIF processors (Firefox, etc.) process missing width (height)
     as screen_width (screen_height). */

	/* If still zero, error. */
	if (width == 0 || height == 0) {
		emit_read_error(gctx, 1, "image has zero width and/or height");
		Gif_MakeImageEmpty(gfi);
		flags = 0;
	}
	/* If position out of range, error. */
	if (left + width > GIF_MAX_SCREEN_WIDTH || top + height > GIF_MAX_SCREEN_HEIGHT) {
		emit_read_error(gctx, 1, "image position and/or dimensions out of range");
		Gif_MakeImageEmpty(gfi);
		flags = 0;
	}
	GIF_DEBUG(("<%ux%u> ", width, height));

	unsigned char packed = readUint8(grr);
	if (packed & 0x80) { /* have a local color table */
		int size = 1 << ((packed & 0x07) + 1);
		if (!(gfi->local = read_color_table(grr, size)))
			return false;
		gfs->has_local_cmaps = true;
		gfi->local->refcount = 1;
	}

	gfi->interlace = (packed & 0x40) != 0;

	/* Keep the compressed data if asked */
	if (flags & GIF_READ_COMPRESSED) {
		if (!read_compressed_image(grr, gfi, flags))
			return false;
		if (flags & GIF_READ_UNCOMPRESSED) {
			Gif_Reader new_grr;
			make_data_reader(&new_grr, gfi->compressed, gfi->compressed_len);
			ok = uncompress_image(gctx, gfi, &new_grr);
		}
	} else if (flags & GIF_READ_UNCOMPRESSED) {
		ok = uncompress_image(gctx, gfi, grr);
	} else {
		/* skip over the image */
		unsigned char buffer[GIF_MAX_BLOCK];
		unsigned int  size;
		while ((size = readUint8(grr)) > 0)
			readChunk(grr, buffer, size);
	}
	return ok;
}

static void
read_graphic_control_extension(Gif_Context *gctx, Gif_Image *gfi,
                               Gif_Reader *grr)
{
	unsigned char crap[GIF_MAX_BLOCK];
	unsigned int  len = readUint8(grr);
	unsigned char packed;

	if (len == 4) {
		/* u8b */ packed = readUint8(grr);
		gfi->disposal    = (packed >> 2) & 0x07;
		gfi->delay       = readUint16(grr);
		gfi->transparent = readUint8(grr);
		/* 0 ~ ok */ len = readUint8(grr);
		if (!(packed & 0x01)) /* transparent color doesn't exist */
			gfi->transparent = -1;
	}
	if (len != 0) {
		emit_read_error(gctx, 1, "bad graphic extension");
		do {
			readChunk(grr, crap, len);
		} while ((len = readUint8(grr)) > 0);
	}
}

static char *
suck_data(Gif_Reader *grr, char *data, unsigned *store_len)
{
	unsigned int size;
	unsigned int total_len = 0;

	while ((size = readUint8(grr)) > 0) {
		Gif_ReArray(data, char, total_len + size + 1);
		if (!data)
			return NULL;
		readChunk(grr, (unsigned char *)data + total_len, size);
		data[(total_len += size)] = 0;
	}
	if ( store_len )
		*store_len = total_len;
	return data;
}

static void
read_unknown_extension(Gif_Reader *grr, Gif_Stream *gfs, Gif_Image *gfi,
                       int kind, char *appname, int applength)
{
	unsigned int    len = 0;
	unsigned int   size = 0;
	unsigned char *data = NULL;
	Gif_Extension *gext = NULL;

	bool skip = false;

	while ((size = readUint8(grr)) > 0) {
		if (skip) {
			skipBytes(grr, size);
			continue;
		}
		Gif_ReArray(data, unsigned char, len + size + 2);
		if (!(skip = !data)) {
			data[len] = size;
			readChunk(grr, data + len + 1, size);
			len += size + 1;
		}
	}
	if (data)
		gext = Gif_NewExtension(kind, appname, applength);
	if (gext) {
		gext->data       = data;
		gext->length     = len;
		gext->free_data  = Gif_Free;
		gext->packetized = 1;
		/* */ data[len]  = 0;
		/* */ skip       = Gif_AddExtension(gfs, gfi, gext);
	} else
		Gif_DeleteArray(data);
}

static void
read_application_extension(Gif_Context *gctx, Gif_Reader *grr, Gif_Stream *gfs, Gif_Image *gfi)
{
	unsigned char buf[GIF_MAX_BLOCK + 1];
	unsigned char len = readUint8(grr);

	readChunk(grr, buf, len);

	/* Read the Netscape loop extension. */
	if (len == 11
		&& (memcmp(buf, "NETSCAPE2.0", 11) == 0
			|| memcmp(buf, "ANIMEXTS1.0", 11) == 0)) {

		if ((len = readUint8(grr)) == 3) {
			/* throw the 1 */  skipBytes(grr, 1); 
			gfs->loopcount   = readUint16(grr);
			/* 0 ~ ok */ len = readUint8(grr);
			if (len > 0)
				emit_read_error(gctx, 1, "bad loop extension");
		} else
			emit_read_error(gctx, 1, "bad loop extension");

		while (len > 0) {
			readChunk(grr, buf, len);
			len = readUint8(grr);
		}
	} else
		read_unknown_extension(grr, gfs, gfi, 0xFF, (char*)buf, len);
}

static bool
read_comment_extension(Gif_Reader *grr, Gif_Image *gfi)
{
	unsigned len;
	char *ext = suck_data(grr, NULL, &len);
	bool ok = true;
	if ( ext ) {
		 ok = Gif_AddCommentTake((
			 gfi->comment ?: (gfi->comment = Gif_NewComment())
		 ), ext, len);
	}
	return ok;
}

static Gif_Stream *
read_gif(Gif_Reader *grr, int flags, const char* landmark,
         Gif_ReadErrorHandler handler)
{
	static char *last_name;

	int unknown_block_type = 0;

	if (readChar(grr) != 'G' ||
		readChar(grr) != 'I' ||
		readChar(grr) != 'F')
		return NULL;

	skipBytes(grr, 3);

	Gif_Context gctx;
	Gif_Stream *gfs;
	Gif_Image  *gfi;

	if (!make_context(&gctx, handler))
		goto done;
	if (!(gfs = gctx.stream = Gif_NewStream()))
		goto done;
	if (!(gfi = gctx.img = Gif_NewImage())) {
		Gif_DeleteStream(gfs);
		goto done;
	}
	gfs->landmark = landmark;

	GIF_DEBUG(("\nGIF "));
	if (!read_logical_screen_descriptor(grr, gfs))
		goto done;
	GIF_DEBUG(("logscrdesc "));

	do {
		unsigned char byte = readUint8(grr);

		switch (byte) {

		case ',': /* image block */
			GIF_DEBUG(("imageread %d ", gfs->nimages));
			gfi->identifier = last_name;
			last_name = NULL;

			if (!Gif_AddImage(gfs, gfi))
				goto done;
			else if (!read_image(&gctx, grr, gfs, gfi, flags)) {
				Gif_RemoveImage(gfs, gfs->nimages - 1);
				gfi = NULL;
				goto done;
			}
			if (!(gfi = gctx.img = Gif_NewImage()))
				goto done;
			break;

		case ';': /* terminator */
			byte = readUint8(grr);
			GIF_DEBUG(("term\n"));
			goto done;

		case '!': /* extension */
			byte = readUint8(grr);
			GIF_DEBUG(("ext(0x%02X) ", byte));
			switch (byte) {

			case 0xCE:
				last_name = suck_data(grr, last_name, NULL);
				break;

			case 0xF9:
				read_graphic_control_extension(&gctx, gfi, grr);
				break;

			case 0xFE:
				if (!read_comment_extension(grr, gfi))
					goto done;
				break;

			case 0xFF:
				read_application_extension(&gctx, grr, gfs, gfi);
				break;

			default  :
				read_unknown_extension(grr, gfs, gfi, byte, NULL, 0);
			}
		case '\0':
			break;
		default:
			if (!unknown_block_type) {
				char buf[256];
				sprintf(buf, "unknown block type %d at file offset %u", byte, grr->pos - 1);
				emit_read_error(&gctx, 1, buf);
			}
			if (++unknown_block_type > 20)
				goto done;
		}
	} while (!grr->is_end);

done:
	/* Move comments and extensions after last image into stream. */
	if (gfs && gfi) {
		Gif_Extension* gfex;
		gfs->end_comment = gfi->comment;
		gfi->comment = NULL;
		gfs->end_extension_list = gfi->extension_list;
		gfi->extension_list = NULL;
		for (gfex = gfs->end_extension_list; gfex; gfex = gfex->next)
			gfex->image = NULL;
	}
	Gif_DeleteImage(gfi);
	Gif_DeleteArray(last_name);
	clear_context(&gctx);
	last_name = NULL;
	gctx.img = NULL;

	if (gfs)
		gfs->errors = gctx.errors[1];
	if (gfs && gctx.errors[1] == 0
		&& !(flags & GIF_READ_TRAILING_GARBAGE_OK)
		&& !grr->is_end)
		emit_read_error(&gctx, 0, "trailing garbage after GIF ignored");
	/* finally, export last message */
	emit_read_error(&gctx, -1, NULL);

	return gfs;
}

Gif_Stream *
Gif_FullReadFile(FILE *file, int flags,
                 const char* landmark, Gif_ReadErrorHandler handler)
{
	Gif_Reader grr;
	if (!file)
		return NULL;
	make_file_reader(&grr, file);
	return  read_gif(&grr, flags, landmark, handler);
}

Gif_Stream *
Gif_FullReadData(const unsigned char *data, unsigned length, int flags,
                 const char* landmark, Gif_ReadErrorHandler handler)
{
	Gif_Reader grr;
	if (!data)
		return NULL;
	make_data_reader(&grr, data, length);
	if (flags &  GIF_READ_CONST_RECORD)
		flags |= GIF_READ_COMPRESSED;
	return  read_gif(&grr, flags, landmark, handler);
}

void
Gif_SetErrorHandler(Gif_ReadErrorHandler handler) {
	default_error_handler = handler;
}

#ifdef GIFSI_COMPILE_CPP
namespace GifSI {
# ifdef USE_FILE_IO
	unsigned ReadFile(GifSI::Stream& stream, const std::string& filename) {
		Gif_FullReadFile
	}
# endif // USE_FILE_IO
	unsigned ReadData(GifSI::Stream& stream, const std::vector& buffer) {
		Gif_FullReadData
	}
} /* namespace GifSI */
#endif //GIFSI_COMPILE_CPP
