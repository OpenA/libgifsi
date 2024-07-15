
#include "gifsi.hpp"

using namespace GifSi;

#define MAX_CODE_BITS 12
#define READ_CODE_MAX 4096
#define READ_BUF_SIZE 256
#define CALC_COLORS_N(p) (1 << ((p & 0x07) + 1))

#define CLEAR_CODE (1 << min_code_size)
#define   EOI_CODE (1 << min_code_size | 1)
#define   BPP_SIZE (1 +  min_code_size)

struct Code {
	unsigned suffix:8, prefix:12, nbits:12;
};

template<class T>
void Image::read_gif_image_data(T& gR, Stream *stm, int imx, exType type)
{
	unsigned char  pack, nb;
	unsigned short loop;
	struct   exDat ex;

	bool skip = false;
	  ex.imdx = imx;
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
		DebugPrint("-- GFX delay=%d transparent=%d,%d",
			m_delay, has_transparent, m_alpha);
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
			stm->addExtension(ex);
		}
		DebugPrint("-- %s #%x loop=%d", ex.data, pack, loop);
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
		   skip  = true;
		// have a local color table
		if (pack & 0x80)
			m_colors.read_color_table(gR, CALC_COLORS_N(pack));
		if (pack & 0x40)
			has_interlace = true;
		if (!(nb = decode_gif_image(gR)))
			return;
		break;
	case exType::Comment:
	case exType::Identifer:
	default: // Unknown
		skip = stm->has(Exclude_ExtGarbage);
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
			stm->addExtension(ex);
		}
	}
# ifdef DEBUG
	DebugPrint(" %s (%s_%s=%d)\n",
		(type == exType::Comment || type == exType::Identifer) &&
			ex.data ? (const char *)ex.data : ";",
		(type == exType::iData) ? "IMAF": "BLOCK",
			skip ? "DROP" : "READ", loop
	);
# endif
}

/* returns the count of decoding bits
 * for increase decode position. */
static int one_code(
	struct Code  dec[], int dpos, int pc,
	unsigned char *img, int imax, int cc, int nc
) {
	int e = dec[cc].suffix, s = e,
		p = dec[cc].prefix, i = 0,
		l = dec[cc].nbits , k = dec[pc].nbits + 1;
	// in curr_code == next_code we need prev_code prefix/nbits
	if (cc == nc)
		p = pc, l = k;

	for(i = l-2; i >= 0; i--) {
		s = dec[p].suffix,
		p = dec[p].prefix;
		if ((dpos+i) < imax)
			img[(dpos+i)] = s;
	}
	// we don't know code's final suffix so we store 
	// all possible values and conditionally stored one of then
	if ((dpos+l-1) < imax)
		img[(dpos+l-1)] = (cc == nc ? (l ? s : 0) : e);
	// set up the prefix and nbits for the next code
	// i think it would be stored like a single word
	dec[nc].suffix = (l ? s : 0);
	dec[nc].prefix = pc;
	dec[nc].nbits  = k;

	return l;
}

/* returns number of bytes in last block with EOI code (end-of-image)
 * so this num is not used for skipping, only for comparing to zero.
 * (zero means that the block does not contain an EOI and image may be incomplete).*/
template<class T> auto Image::decode_gif_image(T& gR) -> int
{
	signed const   outMax = width() * height();
	unsigned char *outBuf = new unsigned char[outMax];

	/* we need a bit more than READ_BUF_SIZE in case a single code is split
		across blocks */
	unsigned char buf[READ_BUF_SIZE + 4];
	unsigned int accm;

	struct Code decTab[READ_CODE_MAX];

	int i, next_code, curr_code, bit_pos = 0, decPos = 0,
		n, prev_code, bits_need, bit_len = 0;

#define BUMP_CODE   (1 << bits_need)
#define CODE_GET(m) (m >> (bit_pos % 8) & (BUMP_CODE - 1))

	int min_code_size = gR.readUint8();
	if (min_code_size >= MAX_CODE_BITS) {
		// too big
		min_code_size = MAX_CODE_BITS - 1;
	} else if (min_code_size < 2) {
		// too small
		min_code_size = 2;
	}
	// initialize `n` and `decTab`
	for (n = i = 0; i < READ_CODE_MAX; i++) {
		decTab[i].prefix = 0xC11;
		decTab[i].suffix = (unsigned char)i;
		decTab[i].nbits  = 1;
	}
	// initialize codes
	bits_need = BPP_SIZE;
	next_code = EOI_CODE;
	curr_code = CLEAR_CODE;
	/* Thus the 'Read in the next data block.' code below will be invoked on the
	   first time through: exactly right! */
	
	DebugPrint("-- IMAGE <%d,%d>%dx%d bpp=%d colors=%d\n====================\nidata blocks decode ",
		m_left, m_top, m_width, m_height, BPP_SIZE, m_colors.count());
	do {

	/* GET A CODE INTO THE 'curr_code' VARIABLE.
	*
	* 9.Dec.1998 - Rather than maintain a byte pointer and a bit offset into
	* the current byte (and the processing associated with that), we maintain
	* one number: the offset, in bits, from the beginning of 'buffer'. This
	* much cleaner choice was inspired by Patrick J. Naughton
	* <naughton@wind.sun.com>'s GIF-reading code, which does the same thing.
	* His code distributed as part of XV in xvgif.c. */

		if ((bit_pos + bits_need) > bit_len) {
			// Read in the next data block.
			if (bit_pos >= 8) {
				// Need to shift down the upper, unused part of `buf`
				i = bit_pos / 8;
				buf[0] = buf[i];
				buf[1] = buf[i+1];
				bit_pos -= i * 8;
				bit_len -= i * 8;
			}
			if ((n = gR.readUint8())) {
				gR.readChunk(n, &buf[bit_len / 8]);
				bit_len += n * 8;
			}
			DebugPrint(".");
			continue;
		}
		i = bit_pos / 8;
		accm  = buf[i],
		accm |= buf[i+1] << 8;
		if (bits_need >= 8)
			accm |= buf[i+2] << 16;

		prev_code = curr_code,
		curr_code = CODE_GET(accm);
		bit_pos  += bits_need;

	/* CHECK FOR SPECIAL OR BAD CODES: clear_code, eoi_code, or a code that is
	* too large. */
		if (curr_code == CLEAR_CODE) {
			DebugPrint("| (%d) CLEAR\n", n);
			bits_need = BPP_SIZE;
			next_code = EOI_CODE;
			continue;
		} else if (curr_code == EOI_CODE) {
			DebugPrint("@ (%d) EOI", n);
			break;
		} else if (curr_code > next_code && next_code && next_code != CLEAR_CODE) {
	/* code > next_code: a (hopefully recoverable) error.

	* Bug fix, 5/27: Do this even if old_code == clear_code, and set code
	* to 0 to prevent errors later. (If we didn't zero code, we'd later set
	* old_code = code; then we had old_code >= next_code; so the prefixes
	* array got all screwed up!)

	* Bug fix, 4/12/2010: It is not an error if next_code == clear_code.
	* This happens at the end of a large GIF: see the next comment ("If no
	* meaningful next code should be defined...."). */
			curr_code = 0;
		}
	/* PROCESS THE CURRENT CODE and define the next code. If no meaningful
	* next code should be defined, then we have set next_code to either
	* 'eoi_code' or 'clear_code' -- so we'll store useless prefix/suffix data
	* in a useless place. */
		decPos += one_code(decTab, decPos, prev_code,
		                   outBuf, outMax, curr_code, next_code);
	// 7.Mar.2014 -- Avoid error if image has zero width/height.
	/* Increment next_code except for the 'clear_code' special case (that's
	 when we're reading at the end of a GIF) */
		if (next_code != CLEAR_CODE && ++next_code == BUMP_CODE) {
			if (bits_need < MAX_CODE_BITS)
				bits_need++;
			else
				next_code = CLEAR_CODE;
		}
	} while (n > 0);

	m_pixels = outBuf;
	m_bpp = BPP_SIZE;
#ifdef DEBUG
	if (!n) DebugPrint(" ; zero block reached (%i miss)\n", (outMax - decPos));
#endif
	return n;
}

template<class T>
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

template<class T>
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
	DebugPrint("\nSCREEN: bg=%d,%dx%d colors=%d\n",
		m_background, m_screenWidth, m_screenHeight, g_colors.count());
	do {
		switch ((pack = gR.readUint8())) {
		case ',': // frame
			DebugPrint("FRAME:%d\n", imx);
			/* read and decode idata blocks for last image on stack
			* */m_images[imx].read_gif_image_data(gR, this, imx, exType::iData);
			if (m_images[imx].hasLocalColors())
				has_local_colors = true;
			imx = addImage();
			break;
		case '!': // extension
			pack = gR.readUint8();
			DebugPrint("\nEXT@%x\n", pack);
			// only F9 needs for img, all others moves to stream
			m_images[imx].read_gif_image_data(gR, this, imx, (
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
