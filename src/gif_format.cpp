
#include "gifsi.hpp"

using namespace GifSi;

#define MAX_CODE_BITS 12
#define READ_CODE_MAX 4096
#define READ_BUF_SIZE 255
#define CALC_COLORS_N(p) (1 << ((p & 0x07) + 1))

template<typename T>
void Image::read_gif_image_data(T& gR, Stream *stm, exType type)
{
	unsigned char  pack, nb;
	unsigned short loop;
	struct   exDat ex;

	bool skip = false;
	  ex.type = type;
	loop=pack = 0;

# define IS_APP_NETSCAPE(b) (b[0]=='N'&& b[1]=='E'&& b[2]=='T'&& b[3]=='S'&& b[4]=='C'&& b[5]=='A'&& b[6]=='P'&& b[7]=='E')
# define IS_APP_ANIMEXTS(b) (b[0]=='A'&& b[1]=='N'&& b[2]=='I'&& b[3]=='M'&& b[4]=='E'&& b[5]=='X'&& b[6]=='T'&& b[7]=='S')

	switch (type) {
	case exType::GfxControl:
		    nb  = gR.readUint8(),
		  skip  = true,
		  pack  = gR.readUint8(),
		m_delay = gR.readUint16(),
		m_alpha = gR.readUint8();
		if ( nb > 4 )
			gR.skipBytes(nb - 4);
		// 0 ~ transparent color doesn't exist
		has_transparent = (pack & 0x01);
		m_disposal      = (pack & 0x1C) >> 2;
# ifdef DEBUG
		printf("-- GFX delay=%d transparent=%d,%d",
			m_delay, has_transparent, m_alpha);
# endif
		break;
	case exType::AppExtend:
		nb      = gR.readUint8(),
		ex.data = gR.dataExtract(nb);
		ex.size = nb;
		// Read the Netscape loop extension.
		if (nb >= 8 && (IS_APP_NETSCAPE(ex.data) || IS_APP_ANIMEXTS(ex.data))) {
			nb   = gR.readUint8(),
			skip = true,
			pack = gR.readUint8(),
			loop = gR.readUint16();
			if (nb > 3)
				gR.skipBytes(nb - 3);
			if (pack & 0x01)
				stm->setLoopCount(loop);
		} else {
			m_raw.push_back(ex);
		}
# ifdef DEBUG
		printf("-- %s #%x loop=%d", ex.data, pack, loop);
# endif
		if (skip)
			gR.dataRelease(ex.data);
		break;
	case exType::iData:
	// Mainline GIF engines (Firefox, etc.) missing image width/height
	// substitute the global screen width/height instead.
		m_left   = gR.readUint16();
		m_top    = gR.readUint16();
		m_width  = gR.readUint16() ?: stm->screenWidth();
		m_height = gR.readUint16() ?: stm->screenHeight();
		   pack  = gR.readUint8();
		m_bpp    = gR.readUint8();
		// have a local color table
		if (pack & 0x80)
			m_colors.read_color_table(gR, CALC_COLORS_N(pack));
		if (pack & 0x40)
			has_interlace = true;
# ifdef DEBUG
		printf("-- IMAGE <%d,%d>%dx%d bpp=%d colors=%d",
			m_left, m_top, m_width, m_height, m_bpp, m_colors.count());
# endif
		break;
	case exType::Comment:
	case exType::Identifer:
	default: // Unknown
		skip = m_cpyflags & Exclude_Extensions;
	}
# ifdef DEBUG
	for (loop = 0; (nb = gR.readUint8()); loop++)
# else
	// scan over image data block by block.
	while (( nb = gR.readUint8() ))
# endif
	{
		if (skip) gR.skipBytes(nb);
		else {
			ex.data = gR.dataExtract(nb);
			ex.size = nb;
			m_raw.push_back(ex);
		}
	}
# ifdef DEBUG
	printf(" %s (%s_%s=%d)\n",
		(type == exType::Comment || type == exType::Identifer) &&
			ex.data ? (const char *)ex.data : ";",
		(type == exType::iData) ? "IDATA": "BLOCK",
			skip ? "DROP" : "READ", loop
	);
# endif
}

template<typename T>
void Image::decode_gif_image() {

	unsigned int nb, npix = (unsigned)m_width * (unsigned)m_height;
	unsigned char *raw;

	for (auto dat : m_raw) {
		nb  = dat.size;
		raw = dat.data;
		if (!(dat.type == exType::iData))
			continue;
	}
}

static auto decode_image_data(
	ExdatList const &raw  , int min_code_size,
	unsigned char outBuf[], const int outMax
) -> eCode {

	// we need a bit more than READ_BUFFER_SIZE in case a single code is split across blocks
	unsigned char buf[READ_BUF_SIZE + 5];
	unsigned int  accm = 0, dec_pix;

	struct {
		unsigned char  suffix, nbits;
		unsigned short prefix;
	} dec[READ_CODE_MAX];

	int i, bit_len, dec_len, curr_code, old_code, clear_code,
	    n, bit_pos, dec_pos, next_code, eoi_code, bits_need;

# define CUR_BUMP_CODE (1 << bits_need)
# define CUR_CODE_CALC(m) ((m >> (bit_pos % 8)) & (CUR_BUMP_CODE - 1))

	for (i = 0; i < 256; i++) {
		dec[i].suffix = i,
		dec[i].nbits  = 1,
		dec[i].prefix = 0xC114;
	}
	if (min_code_size >= MAX_CODE_BITS) {
		// too big
		min_code_size = MAX_CODE_BITS - 1;
	} else if (min_code_size < 2) {
		// too small
		min_code_size = 2;
	}
	bits_need  = (1 + min_code_size),
	clear_code = curr_code = (1 << min_code_size),
	 next_code = eoi_code  = (clear_code + 1);

	/* Thus the 'Read in the next data block.' code below will be invoked on the
	   first time through: exactly right! */
	while (1) {

	/* GET A CODE INTO THE 'code' VARIABLE.
	*
	* 9.Dec.1998 - Rather than maintain a byte pointer and a bit offset into
	* the current byte (and the processing associated with that), we maintain
	* one number: the offset, in bits, from the beginning of 'buffer'. This
	* much cleaner choice was inspired by Patrick J. Naughton
	* <naughton@wind.sun.com>'s GIF-reading code, which does the same thing.
	* His code distributed as part of XV in xvgif.c. */

		if ((bit_pos + bits_need) > bit_len) {
			// Read in the next data block.
			for (; (bit_pos + bits_need) > bit_len; bit_len += n * 8) {
				// Read in the next data block.
				if (bit_pos >= 8) {
					// Need to shift down the upper, unused part of 'buffer'
					i = bit_pos / 8;
					buf[0] = buf[i];
					buf[1] = buf[i + 1];
					bit_pos -= i * 8;
					bit_len -= i * 8;
				}
				//if (!(n = gR.readUint8())) {
				//	b.pos = b.len = -1;
				//	break;
				//} else
				//	gR.readChunk(nb, &buf[b.len / 8]);
			}
			//b = read_image_block(gR, bits_need, buffer, b);
			//if (b.len == -1 || b.pos == -1)
			//	goto zero_length_block;
		}
		i = bit_pos / 8;
		accm  = buf[i];
		accm |= buf[i+1] << 8;
		if (bits_need >= 8)
			accm |= buf[i+2] << 16;
		 bit_pos += bits_need;
		 old_code = curr_code,
		curr_code = CUR_CODE_CALC(accm);

	// Check for special/bad codes:
	// clear, eoi, or a code that is too large.
		if (curr_code == clear_code) {
			next_code = eoi_code;
			bits_need = min_code_size + 1;
			continue;
		} else if (curr_code == eoi_code) {
			break;
		} else if (curr_code > next_code && next_code && next_code != clear_code) {
	// code > next_code: a (hopefully recoverable) error.

	// Bug fix, 5/27: Do this even if old_code == clear_code, and set code
	// to 0 to prevent errors later. (If we didn't zero code, we'd later set
	// old_code = code; then we had old_code >= next_code; so the prefixes
	// array got all screwed up!)

	// Bug fix, 4/12/2010: It is not an error if next_code == clear_code.
	// This happens at the end of a large GIF: see the next comment
	// (If no meaningful next code should be defined....).
			curr_code = 0;
		}
	// PROCESS THE CURRENT CODE and define the next code. If no meaningful
	// next code should be defined, then we have set next_code to either
	// `eoi_code` or `clear_code` -- so we'll store useless prefix/suffix data
	// in a useless place. */

	// First, set up the prefix and length for the next code
	// (in case `curr_code` == `next_code`).
		dec[next_code].prefix = old_code;
		dec[next_code].nbits  = dec[old_code].nbits + 1;

	// Use one_code to process code. It's nice that it returns the first
	// pixel in code: that's what we need.
	 	dec_len = dec[curr_code].nbits;
		dec_pix = dec[curr_code].prefix;

	// get the first pixel in the code, which, since we walked backwards
	// through the code, was the last suffix we processed.
		for(n = i = 0; i < dec_len; i++, dec_pos++) {
			n = dec[curr_code + i].prefix;
			if (outMax > dec_pos)
				outBuf[dec_pos] = dec[n].suffix;
		}
	// Special processing if code == next_code: we didn't know code's final
	// suffix when we called one_code, but we do now.
	// 7.Mar.2014 -- Avoid error if image has zero width/height.
		if (curr_code == next_code && (dec_pos - 1) < outMax)
			outBuf[dec_pos - 1] = dec_pix;

	// Increment next_code except for the `clear_code` special case
	// (that's  when we're reading at the end of a GIF)
		if (next_code != clear_code && (next_code += 1) == CUR_BUMP_CODE) {
			if (bits_need < MAX_CODE_BITS)
				bits_need++;
			else
				next_code = clear_code;
		}
	}
	// zero-length block reached.
	//zero_length_block: {
	//	long delta = (long)(gctx->maximage - gctx->image) - (long)decode_pos;
	//	char buf[READ_BUFFER_SIZE];
	//	if (delta > 0) {
	//		sprintf(buf, "missing %ld %s of image data", delta,
	//				delta == 1 ? "pixel" : "pixels");
	//		emit_read_error(gctx, GE_Error, buf);
	//		memset(&gctx->image[decode_pos], 0, delta);
	//	} else if (delta < -1) {
	//		// One pixel of superfluous data is OK; that could be the
	//		// code == next_code case.
	//		sprintf(buf, "%ld superfluous pixels of image data", -delta);
	//		emit_read_error(gctx, GE_Warning, buf);
	//	}
	//}
# undef CUR_BUMP_CODE
# undef CUR_CODE_CALC
}

template<typename T>
void Colormap::read_color_table(T &gR, int ncol)
{
	Color col;
	for(int i = 0; i < ncol; i++) {
		col.r = gR.readUint8();
		col.g = gR.readUint8();
		col.b = gR.readUint8();
		col.a = 0;
		m_map.push_back(col);
	}
}

template<typename T>
auto Stream::read_gif_stream(T &gR) -> int
{
	unsigned char pack, unk_block = 0;
	int  imx  = addImage(), ecode = 0;
	bool done = false;

	// don't care about screen w/h
	m_screenWidth  = gR.readUint16();
	m_screenHeight = gR.readUint16();
	        pack   = gR.readUint8();
	m_background   = gR.readUint8();
	// don't care about pixel aspect ratio
	gR.skipBytes(1);
	// have a global color table
	if (pack & 0x80) { 
		g_colors.read_color_table(gR, CALC_COLORS_N(pack));
		has_bg_color = true;
	}
# ifdef DEBUG
	printf("\nSCREEN: bg=%d,%dx%d colors=%d\n",
		m_background, m_screenWidth, m_screenHeight, g_colors.count());
# endif
	do {
		switch ((pack = gR.readUint8())) {
		case ',': // frame
# ifdef DEBUG
			printf("FRAME:%d\n", imx);
# endif
			/**/m_images[imx].read_gif_image_data(gR, this, exType::iData);
			if (m_images[imx].hasLocalColors())
				has_local_colors = true;
			imx = addImage();
			break;
		case '!': // extension
			pack = gR.readUint8();
# ifdef DEBUG
			printf("\nEXT@%x\n", pack);
# endif
			m_images[imx].read_gif_image_data(gR, this, (
				pack == 0xCE ? exType::Comment    :
				pack == 0xFE ? exType::Identifer  :
				pack == 0xF9 ? exType::GfxControl :
				pack == 0xFF ? exType::AppExtend  :
				               exType::Unknown));
			break;
		case ';': // terminator
			pack = gR.readUint8();
			done = true;
			break;
		case '\0':
			break;
		default:
			if (++unk_block > 20) {
				ecode = 309;
				done = true;
			}
		}
	} while (!gR.isEnd() && !done);

	if ( m_images[imx].hasEmpty())
		delImage();
	return ecode;
}
