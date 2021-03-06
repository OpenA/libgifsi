/* gifread.c - Functions to read GIFs.
   Copyright (C) 1997-2019 Eddie Kohler, ekohler@gmail.com
   This file is part of the LCDF GIF library.

   The LCDF GIF library is free software. It is distributed under the GNU
   General Public License, version 2; you can copy, distribute, or alter it at
   will, as long as this notice is kept intact and this source code is made
   available. There is no warranty, express or implied. */

#include <gifsi.h>

#define READ_CODE_MAX    0x1000
#define READ_CODE_BITS   12
#define READ_BUFFER_SIZE 255

typedef unsigned short Code_t;

typedef struct Gif_Reader Gif_Reader;
typedef struct GReadContext GReadContext;

struct GReadContext {

	Gif_Stream    *stream;

	Code_t         prefix[READ_CODE_MAX];
	unsigned char  suffix[READ_CODE_MAX];
	unsigned short length[READ_CODE_MAX];

	unsigned short width, height;
	unsigned char *image, *maximage;
	unsigned int   index, decodepos;
	unsigned short errors, warnings;
};

#define SET_GRCntxDefaults(gst) {\
	.errors = 0, .warnings = 0,\
	.index  = 0, .stream   = gst\
}

struct Gif_Reader {
#if WITH_FILE_IO
	FILE *file;
#endif
	const unsigned char *data;
	const unsigned int length;

	unsigned pos;
	bool is_end;

	unsigned char (*Read_byte )(Gif_Reader *);
	unsigned int  (*Read_chunk)(Gif_Reader *, unsigned char *, unsigned);
	void          (*Skip_bytes)(Gif_Reader *, unsigned );
	bool          (*Read_image)(Gif_Reader *, Gif_Image *, bool);
};

#define readUint8(grr)        ((*grr->Read_byte )(grr))
#define readUint16(grr)       ((*grr->Read_byte )(grr) | (*grr->Read_byte)(grr) << 8)
#define skipBytes(grr,off)    ((*grr->Skip_bytes)(grr, off))
#define readChunk(grr,buf,sz) ((*grr->Read_chunk)(grr, buf, sz))
#define readImage(grr,gim,ok) ((*grr->Read_image)(grr, gim, ok))

#define SET_ReaderDefaults(l) {\
	.pos = 0, .is_end = false, .length = l \
}

#if WITH_FILE_IO
static void
skip_file_bytes(Gif_Reader *grr, unsigned offset)
{
	fseek(grr->file, offset, SEEK_CUR);
	if (feof(grr->file) != 0) {
		grr->is_end = true;
		grr->pos = ftell(grr->file);
	} else
		grr->pos += offset;
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

static bool
read_file_image_compressed(Gif_Reader *grr, Gif_Image *gim, bool is_record)
{
	/* non-record; have to read it block by block. */
	unsigned len = 1, cap = 1024;
	unsigned char size, *raw_data = Gif_NewArray(unsigned char, cap);

	if (!raw_data)
		return false;

	/* min code size */
	raw_data[0] = readUint8(grr);

	while ((size = readUint8(grr)) > 0) {
		/* add 2 before check so we don't have to check after loop when appending
		   0 block */
		if (len + size + 2 > cap) {
			cap *= 2;
			if (!Gif_ReArray(raw_data, unsigned char, cap))
				return false;
		}
		raw_data[len] = size;
		readChunk(grr, &raw_data[len + 1], size);
		len += size + 1;
	}
	gim->compressed_len    = len + 1;
	gim->compressed_errors = raw_data[len] = 0;
	gim->compressed        = raw_data;
	return true;
}

static void
make_file_reader(Gif_Reader *grr, FILE *file)
{
	grr->file       = file;
	grr->Read_image = read_file_image_compressed;
	grr->Read_byte  = read_file_byte;
	grr->Read_chunk = read_file_chunk;
	grr->Skip_bytes = skip_file_bytes;
}
#endif

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
	if (grr->pos + offset >= grr->length) {
		grr->pos = grr->length;
		grr->is_end = true;
	} else
		grr->pos += offset;
}

static bool
read_data_image_compressed(Gif_Reader *grr, Gif_Image *gim, bool has_const_record)
{
	unsigned start_pos = grr->pos,
	     size, end_pos = grr->pos + 1; /* skip min code size */

	/* scan over image */
	while (end_pos < grr->length) {
		unsigned char amt = grr->data[end_pos];
		end_pos += amt + 1;
		if (amt == 0)
			break;
	}
	if (end_pos > grr->length)
		end_pos = grr->length;

	grr->pos = end_pos;
	gim->compressed_len = (size = end_pos - start_pos);
	gim->compressed_errors = 0;

	if (has_const_record) {
		gim->compressed = (unsigned char *)&grr->data[start_pos];
	} else
	if ((gim->compressed = Gif_NewArray(unsigned char, size))) {
		memcpy(gim->compressed, &grr->data[start_pos], size);
	} else
		return false;
	return true;
}

static void
make_data_reader(Gif_Reader *grr, const unsigned char *data)
{
	grr->data       = data;
	grr->Read_image = read_data_image_compressed;
	grr->Read_byte  = read_data_byte;
	grr->Read_chunk = read_data_chunk;
	grr->Skip_bytes = skip_data_bytes;
}

static void
emit_read_error(GReadContext *gctx, Gif_eLevel flag, const char *txt)
{
	gctx->warnings += (flag == GE_Warning);
	gctx->errors   += (flag == GE_Error);

	if (gctx->stream->errors.lvl <= flag && gctx->stream->handler) {
		Gif_Error err = {
			.lvl = flag,
			.num = gctx->index,
			.msg = txt
		};
		gctx->stream->handler(gctx->stream, GmE_Read, err);
	}
}

static unsigned char
one_code(GReadContext *gctx, Code_t code)
{
	int lastsuffix = 0;
	int codelength = gctx->length[code];

	unsigned char *suffx = gctx->suffix;
	unsigned char *ptr   = gctx->image + (gctx->decodepos += codelength);
	Code_t        *prefx = gctx->prefix;

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
read_image_data(GReadContext *gctx, Gif_Reader *grr)
{
	/* we need a bit more than READ_BUFFER_SIZE in case a single code is split
		across blocks */
	unsigned char buffer[READ_BUFFER_SIZE + 5];
	unsigned int  accum;
	int i;
	
	int bits_needed  = 0,
	    bit_position = 0,
	    bit_length   = 0;

	Code_t code, old_code, next_code, eoi_code, clear_code;

#define CUR_BUMP_CODE ( 1 << bits_needed)
#define CUR_CODE_MASK ((1 << bits_needed) - 1)

	int min_code_size = readUint8(grr);

	GIF_DEBUG(("\n\nmin_code_size(%d) ", min_code_size));

	if (min_code_size >= READ_CODE_BITS) {
		emit_read_error(gctx, GE_Error, "image corrupted, min_code_size too big");
		min_code_size = READ_CODE_BITS - 1;
	} else if (min_code_size < 2) {
		emit_read_error(gctx, GE_Error, "image corrupted, min_code_size too small");
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
		code = (Code_t)((accum >> (bit_position % 8)) & CUR_CODE_MASK);
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
			if (gctx->errors < 20)
				emit_read_error(gctx, GE_Error, "image corrupted, code out of range");
			else if (gctx->errors == 20)
				emit_read_error(gctx, GE_Error, "(not reporting more errors)");
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
				if (bits_needed < READ_CODE_BITS)
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
		char buf[READ_BUFFER_SIZE];
		if (delta > 0) {
			sprintf(buf, "missing %ld %s of image data", delta,
					delta == 1 ? "pixel" : "pixels");
			emit_read_error(gctx, GE_Error, buf);
			memset(&gctx->image[gctx->decodepos], 0, delta);
		} else if (delta < -1) {
			/* One pixel of superfluous data is OK; that could be the
				code == next_code case. */
			sprintf(buf, "%ld superfluous pixels of image data", -delta);
			emit_read_error(gctx, GE_Warning, buf);
		}
	}
}

static Gif_Colormap *
read_color_table(Gif_Reader *grr, const int ncol)
{
	Gif_Colormap *gfcm = Gif_New(Gif_Colormap);
	if (Gif_InitColormap(gfcm, ncol, ncol)) {
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
uncompress_image(GReadContext *gctx, Gif_Image *gfi, Gif_Reader *grr)
{
	if (!Gif_CreateUncompressedImage(gfi, gfi->interlace))
		return false;
	gctx->width    = gfi->width;
	gctx->height   = gfi->height;
	gctx->image    = gfi->image_data;
	gctx->maximage = gfi->image_data + (unsigned)gfi->width * (unsigned)gfi->height;
	int old_errors = gctx->errors;
	read_image_data(gctx, grr);
	gfi->compressed_errors = gctx->errors - old_errors;
	return true;
}


int Gif_FullUncompressImage(Gif_Stream *gst, Gif_Image *gfi, char read_flags)
{
	GReadContext gctx = SET_GRCntxDefaults(gst);
	Gif_Reader   grr  = SET_ReaderDefaults(gfi->compressed_len);
	int ok = 0;

	/* return right away if image is already uncompressed. this might screw over
		people who expect re-uncompressing to restore the compressed version. */
	if (gfi->img)
		return 2;
	if (gfi->image_data)
		/* we have uncompressed data, but not an 'img' array;
		this shouldn't happen */
		return 0;

	gctx.index = Gif_GetIndexOfImage(gst, gfi);

	if (gfi->compressed) {
		make_data_reader(&grr, gfi->compressed);
		ok = uncompress_image(&gctx, gfi, &grr);
	}

	if (gctx.warnings || gctx.errors)
		emit_read_error(&gctx, GE_Log, NULL);
	return ok && !gctx.errors;
}

static int
read_image(GReadContext *gctx, Gif_Reader *grr, Gif_Stream *gst, Gif_Image *gim, char flags)
{
	bool ok = true;
	const unsigned left   = readUint16(grr);
	const unsigned top    = readUint16(grr);
	const unsigned width  = readUint16(grr) ?: gst->screen_width;
	const unsigned height = readUint16(grr) ?: gst->screen_height;
	unsigned char  packed = readUint8 (grr);
  /* Mainline GIF processors (Firefox, etc.) process missing width (height)
     as screen_width (screen_height). */

	/* If still zero, error. */
	if (width == 0 || height == 0) {
		emit_read_error(gctx, GE_Error, "image has zero width and/or height");
		flags = 0;
	}
	/* If position out of range, error. */
	if (left + width > GIF_MAX_SCREEN_WIDTH || top + height > GIF_MAX_SCREEN_HEIGHT) {
		emit_read_error(gctx, GE_Error, "image position and/or dimensions out of range");
		flags = 0;
	}
	GIF_DEBUG(("<%ux%u> ", width, height));

	if (packed & 0x80) { /* have a local color table */
		int size = 1 << ((packed & 0x07) + 1);
		if (!(gim->local = read_color_table(grr, size)))
			return false;
		gst->has_local_colors = true;
		gim->local->refcount = 1;
	}
	gim->interlace = (packed & 0x40) != 0;
	gim->left = left, gim->width = width;
	gim->top  = top, gim->height = height;

	/* Keep the compressed data if asked */
	if (flags & GIF_READ_IMAGE_RAW) {
		if (!readImage(grr, gim, flags & GIF_READ_IMAGE_RAW_CONST))
			return false;
		if (flags & GIF_READ_IMAGE_DECODED) {
			Gif_Reader cdr = SET_ReaderDefaults(gim->compressed_len);
			make_data_reader(&cdr, gim->compressed);
			ok = uncompress_image(gctx, gim, &cdr);
		}
	} else if (flags & GIF_READ_IMAGE_DECODED) {
		ok = uncompress_image(gctx, gim, grr);
	} else {
		/* skip min code size */
		skipBytes(grr, 1);
		/* skip over the image */
		while ((packed = readUint8(grr)))
			skipBytes(grr, packed);
		Gif_MakeImageEmpty(gim);
	}
	return ok;
}

static void
read_graphic_control_extension(GReadContext *gctx, Gif_Image *gfi, Gif_Reader *grr)
{
	unsigned char len = readUint8(grr);
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
		emit_read_error(gctx, GE_Error, "bad graphic extension");
		do {
			skipBytes(grr, len);
		} while ((len = readUint8(grr)));
	}
}

static void
read_name_extension(Gif_Reader *grr, Gif_Image *gim)
{
	unsigned char bsize;
	unsigned int  pos = 0;

	while ((bsize = readUint8(grr))) {
		unsigned len = pos + bsize;
		if (!Gif_ReArray(gim->identifier, char, len + 1))
			return;
		readChunk(grr, &gim->identifier[pos], bsize);
		gim->identifier[len] = '\0';
		pos = len;
	}
}

static void
read_unknown_extension(Gif_Reader *grr, Gif_Stream *gfs, Gif_Image *gfi,
                       short kind, const char *appname, unsigned applength)
{
	unsigned int    len = 0;
	unsigned char  size = 0;
	unsigned char *data = NULL;
	Gif_Extension *gext = NULL;

	bool skip = false;

	while ((size = readUint8(grr)) > 0) {
		if (skip) {
			skipBytes(grr, size);
			continue;
		}
		if (Gif_ReArray(data, unsigned char, len + size + 2)) {
			data[len] = size;
			readChunk(grr, data + len + 1, size);
			len += size + 1;
		} else
			skip = true;
	}
	if (data)
		gext = Gif_New(Gif_Extension);
	if (Gif_InitExtension(gext, kind, appname, applength)) {
		gext->data       = data;
		gext->length     = len;
		gext->packetized = true;
		gext->data[len]  = 0;
		Gif_AddImageExtension(gfi, gext);
	} else
		Gif_DeleteArray(data);
}

static void
read_application_extension(GReadContext *gctx, Gif_Reader *grr, Gif_Stream *gfs, Gif_Image *gfi)
{
	unsigned char buf[READ_BUFFER_SIZE + 1];
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
				emit_read_error(gctx, GE_Error, "bad loop extension");
		} else
			emit_read_error(gctx, GE_Error, "bad loop extension");

		while (len) {
			skipBytes(grr, len);
			len = readUint8(grr);
		}
	} else
		read_unknown_extension(grr, gfs, gfi, 0xFF, (char*)buf, len);
}

static void
read_comment_extension(GReadContext *gctx, Gif_Reader *grr, Gif_Image *gim)
{
	unsigned char *comm_str, comm_len = readUint8(grr);

	if (comm_len > 0) {
		if (!gim->comment)
			Gif_NewComment(gim->comment);
		do {
			comm_str = Gif_NewArray(unsigned char, comm_len);
			readChunk(grr, comm_str, comm_len);
			(void)Gif_CatIndent(gim->comment, comm_str, comm_len);
		} while ((comm_len = readUint8(grr)));
	}
}

static bool
read_gif(Gif_Reader *grr, Gif_Stream *gst, char flags)
{
	int unknown_block_type = 0;

	GReadContext gctx = SET_GRCntxDefaults(gst);

	GIF_DEBUG("read magic `GIF`");
	if (readUint8(grr) != 'G' ||
		readUint8(grr) != 'I' ||
		readUint8(grr) != 'F') {
		emit_read_error(&gctx, GE_Error, "this is not gif image");
		return false;
	}
	skipBytes(grr, 3);

	Gif_Image *last_gim;

	if (!Gif_NewImage(last_gim) || !read_logical_screen_descriptor(grr, gst))
		goto done;

	do {
		unsigned char byte = readUint8(grr);

		switch (byte) {

		case ',': /* image block */
			GIF_DEBUG(("imageread %d ", gst->nimages));

			if (Gif_PutImage(gst, last_gim) == -1)
				goto done;
			else if (!read_image(&gctx, grr, gst, last_gim, flags)) {
				Gif_RemoveImage(gst, -1);
				last_gim = NULL;
				goto done;
			}
			gctx.index = gst->nimages;
			if (!Gif_NewImage(last_gim))
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
				read_name_extension(grr, last_gim);
				break;

			case 0xF9:
				read_graphic_control_extension(&gctx, last_gim, grr);
				break;

			case 0xFE:
				read_comment_extension(&gctx, grr, last_gim);
				break;

			case 0xFF:
				read_application_extension(&gctx, grr, gst, last_gim);
				break;

			default  :
				read_unknown_extension(grr, gst, last_gim, byte, NULL, 0);
			}
		case '\0':
			break;
		default:
			if (!unknown_block_type) {
				char buf[READ_BUFFER_SIZE];
				sprintf(buf, "unknown block type %d at file offset %u", byte, grr->pos - 1);
				emit_read_error(&gctx, GE_Error, buf);
			}
			if (++unknown_block_type > 20)
				goto done;
		}
	} while (!grr->is_end);

done:
	/* Move comments and extensions after last image into stream. */
	if (last_gim) {
		gst->end_extension_list  = last_gim->extension_list;
		gst->end_comment         = last_gim->comment;
		last_gim->extension_list = NULL;
		last_gim->comment        = NULL;
	}
	Gif_DeleteImage(last_gim);

	if (!(gst->errors.num = gctx.errors)
		&& !(flags & GIF_READ_TRAILING_GARBAGE_OK)
		&& !grr->is_end)
		emit_read_error(&gctx, GE_Warning, "trailing garbage after GIF ignored");
	/* finally, export last message */
	emit_read_error(&gctx, GE_Log, "Reading GIF complete successfully!");
	/* test copy chain
	Gif_Stream *newgfs = Gif_New(Gif_Stream);
	Gif_CopyStream(newgfs, gfs, NO_COPY_GIF_EXTENSIONS | NO_COPY_GIF_COMMENTS);
	Gif_FreeStream(gfs);
	Gif_RemoveImage(gfs = newgfs, -1); */

	return true;
}

#if WITH_FILE_IO
bool Gif_FullReadFile(Gif_Stream *gst, char read_flags, FILE *file)
{
	Gif_Reader grr = SET_ReaderDefaults(0);
	if (!file || !gst)
		return false;
	make_file_reader(&grr, file);
	return  read_gif(&grr, gst, read_flags);
}
#endif

bool Gif_FullReadData(Gif_Stream *gst, char read_flags, const unsigned char *data, unsigned len)
{
	Gif_Reader grr = SET_ReaderDefaults(len);
	if (!data || !gst)
		return false;
	make_data_reader(&grr, data);
	return  read_gif(&grr, gst, read_flags);
}
