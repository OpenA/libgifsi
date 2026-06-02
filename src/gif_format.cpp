
#include "gifsi.hpp"
#include "gifsi_io.hpp"

using namespace GifSi;

/* Read specification
 * https://www.w3.org/Graphics/GIF/spec-gif89a.txt
*/
struct Code {

	const enum BitRange {
		MinCodeBits = 2,
		MaxCodeBits = 12,
		MaxCodeRead = 4096
	} m_bpp;

	struct {
		unsigned char  suffix, nbits;
		unsigned short prefix;
	} m_tab[MaxCodeRead];

	signed int m_pos;

	Code(int min_code_size = 0) : m_bpp(
		min_code_size >= MaxCodeBits ? (BitRange)(MaxCodeBits-1) :
		min_code_size <  MinCodeBits ? MinCodeBits : (BitRange)min_code_size
	) {
		// initialize `decoder Tab`
		for (int i = 0; i < MaxCodeRead; i++) {
			m_tab[i].prefix = 0xC11;
			m_tab[i].suffix = i;
			m_tab[i].nbits  = 1;
		}
		m_pos = 0;
	}

	auto _BPP  () -> int { return (1 +  m_bpp); }
	auto _CLEAR() -> int { return (1 << m_bpp); }
	auto _EOI  () -> int { return (1 << m_bpp | 1); }

	template<class R> static int dec_gif_image(Image&, R&);
	/*~~~~~~~~~~~~~~~~~~~*/ void dec_one_pixel(Image&, int, int, int);
};

struct Pack {
	unsigned char n_value, b_size;

	bool scan_next;

	auto n_colors() -> unsigned short { return      1 << ((n_value & 0x07) + 1); };
	auto disposal() -> enum Disposal  { return (Disposal)((n_value & 0x1C) >> 2); }

	bool hasColorTable() const { return n_value & 0x80; }
	bool hasInterlace () const { return n_value & 0x40; }
	bool hasAlphaColor() const { return n_value & 0x01; }
	bool hasLoopsLimit() const { return n_value & 0x01; }

	void addNColors(unsigned int nc) { n_value |= (nc - 1) >> 8; }
};

union AppExt {
	struct { unsigned long long lnum; };
	struct { unsigned char buf[8]; };
	struct { const    char str[8]; };

	bool operator==(AppExt other) {
		return lnum == other.lnum;
	}
	bool operator==(const char *lit) {
		for (int i = 0; i < 8; i++) {
			if (str[i] != lit[i])
				return false;
		}
		return true;
	}
};

template<class T>
void Stream::read_gif_image_data(T& gR, int idx, unsigned int type)
{
	auto ext = AppExt{0};
	auto pck = Pack{0,0,true};
	auto &frm = g_frames.at(idx);
	auto &img = frm.image;

	switch (type) {
	case 0xF9: // Graphics Control
		pck.b_size  = gR.readUint8 ();
		pck.n_value = gR.readUint8 ();
		img.m_delay = gR.readUint16();
		img.m_alpha = gR.readUint8 ();
		// 0 ~ transparent color doesn't exist
		img.m_prop.transparent = pck.hasAlphaColor();
		img.m_prop.disposal    = pck.disposal();

		DebugLog("-- GFX delay=%d alpha=%d transparent=%d disposal=%d\n",
			img.m_delay, img.m_alpha, pck.hasAlphaColor(), pck.disposal());
		if (pck.b_size > 4)
			gR.skipBytes(pck.b_size - 4);
		break;
	case 0xFF: // App Extension
		if ((pck.b_size = gR.readUint8()) >= 8) {
			gR.readChunk(8, ext.buf);
			gR.skipBytes(pck.b_size - 8);
			// Read the Netscape loop extension.
			if (ext == "NETSCAPE" || ext == "ANIMEXTS") {
				pck.b_size   = gR.readUint8();
				pck.n_value  = gR.readUint8();
				g_loopsCount = gR.readUint16();
				if (pck.b_size > 3)
					gR.skipBytes(pck.b_size - 3);
				if (pck.hasLoopsLimit())
					g_flags.has_limit_loops = true;
			}
			DebugLog("-- %s loops=%d,%d\n", ext.buf, g_loopsCount, pck.hasLoopsLimit());
		} else
			gR.skipBytes(pck.b_size);
		break;
	case 0x100:
	// Mainline GIF engines (Firefox, etc.) missing image width/height
	// substitute the global screen width/height instead.
		img.m_rect.x = gR.readUint16();
		img.m_rect.y = gR.readUint16();
		img.m_rect.w = gR.readUint16() ?: g_screenWidth;
		img.m_rect.h = gR.readUint16() ?: g_screenHeight;
		pck.n_value  = gR.readUint8();
		// interlaced image
		if (pck.hasInterlace())
			img.m_prop.interlace = true;
		// have a local color table
		if (pck.hasColorTable()) {
			g_flags.has_local_colors = true;
			img.m_sic = g_colors.size();
			img.m_eic = pck.n_colors() - 1;
			read_gif_color_table(gR, pck.n_colors());
		}
		img.m_bpp = gR.readUint8();
		frm.setup(Frame::TypeImage, img.size());
		// ~~
		DebugLog("-- IMAGE <%d,%d>%dx%d colors=%d\n",
			img.left(), img.top(), img.width(), img.height(), pck.n_colors());
		if (!(pck.b_size = Code::dec_gif_image(frm.image, gR)))
			pck.scan_next = false;
		break;
	case 0xFE: // Identifer
	case 0xCE: // Comment
		break;
	default:   // Unknown
		pck.scan_next = true;
	}
	// scan over image data block by block.
	for(ext.lnum = 0; pck.scan_next && (pck.b_size = gR.readUint8());) {
		ext.lnum += ( pck.b_size );
		gR.skipBytes( pck.b_size );
	}
# ifdef DEBUG
	if (pck.scan_next)
		DebugLog(type == 0x100 ? "..v [%lld bytes after end]\n" : "  |-- bytes_drop=%lld\n", ext.lnum);
# endif
}

/* returns the count of decoding bits
 * for increase decode position. */
void Code::dec_one_pixel(Image &pix, int pc, int cc, int nc)
{
	int e = m_tab[cc].suffix, s = e,
		p = m_tab[cc].prefix, i = 0,
		l = m_tab[cc].nbits,
		k = m_tab[pc].nbits + 1;
	// in curr_code == next_code we need prev_code prefix/nbits
	if (cc == nc)
		p = pc, l = k;

	for(i = l-2; i >= 0; i--) {
		s = m_tab[p].suffix,
		p = m_tab[p].prefix;
		if ((m_pos+i) < pix.size())
			pix[(m_pos+i)] = s;
	}
	// we don't know code's final suffix so we store 
	// all possible values and conditionally stored one of then
	m_pos += l;
	if ((m_pos-1) < pix.size())
		pix[(m_pos-1)] = (cc == nc ? (l ? s : 0) : e);
	// set up the prefix and nbits for the next code
	// i think it would be stored like a single word
	m_tab[nc].suffix = (l ? s : 0);
	m_tab[nc].prefix = pc;
	m_tab[nc].nbits  = k;
}

/* returns number of bytes in last block with EOI code (end-of-image)
 * so this num is not used for skipping, only for comparing to zero.
 * (zero means that the block does not contain an EOI and image may be incomplete).*/
template<class T> int Code::dec_gif_image(Image& pix, T& gR)
{
	auto dTab = Code(pix.bpp());

	/* we need a bit more than READ_BUF_SIZE in case a single code is split
		across blocks */
	unsigned char buf[DataWriter::MaxBlockSize + 4];
	unsigned int accm;

	int i, next_code, curr_code, bit_pos = 0,
	    n, prev_code, bits_need, bit_len = 0;

#define BUMP_CODE   (1 << bits_need)
#define CODE_GET(m) (m >> (bit_pos % 8) & (BUMP_CODE - 1))

	i = n = accm = 0;
	// initialize codes
	bits_need = dTab._BPP();
	next_code = dTab._EOI();
	curr_code = dTab._CLEAR();
	/* Thus the 'Read in the next data block.' code below will be invoked on the
	   first time through: exactly right! */
	DebugLog("=======(bpp:%d)========|\n idata blocks decode ", dTab._BPP());
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
			DebugLog(".");
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
		if (curr_code == dTab._CLEAR()) {
			DebugLog("| (%d) CLEAR\n", n);
			bits_need = dTab._BPP();
			next_code = dTab._EOI();
			continue;
		} else if (curr_code == dTab._EOI()) {
			DebugLog("@ (%d) EOI\n", n);
			break;
		} else if (curr_code > next_code && next_code && next_code != dTab._CLEAR()) {
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
		dTab.dec_one_pixel(pix, prev_code, curr_code, next_code);
	// 7.Mar.2014 -- Avoid error if image has zero width/height.
	/* Increment next_code except for the 'clear_code' special case (that's
	 when we're reading at the end of a GIF) */
		if (next_code != dTab._CLEAR() && (next_code += 1) == BUMP_CODE) {
			if (bits_need < Code::MaxCodeBits)
				bits_need++;
			else
				next_code = dTab._CLEAR();
		}
	} while (n > 0);

#ifdef DEBUG
	DebugLog("..x [%i missed idata blocks]\n", (pix.size() - dTab.m_pos));
#endif
	return n;
}

template<class T>
void Stream::read_gif_color_table(T &gR, int ncol)
{
	Color col;
	for(int i = 0; i < ncol; i++) {
		col.r = gR.readUint8();
		col.g = gR.readUint8();
		col.b = gR.readUint8();
		col.a = 0;
		g_colors.push_back(col);
	}
}

template<class T>
auto Stream::read_gif_stream(T &gR) -> eStatus
{
	auto pack = Pack{0, 0, true};
	auto ecode = eStatus::EvrethingOK;

	int n_bg_colors, unk_block = 0, idx = addFrame();

	// don't care about screen w/h
	g_screenWidth  = gR.readUint16();
	g_screenHeight = gR.readUint16();
	pack.n_value   = gR.readUint8 ();
	g_background   = gR.readUint8 ();
	n_bg_colors    = pack.n_colors();
	// don't care about pixel aspect ratio
	gR.skipBytes(1);
	// have a global color table
	if (pack.hasColorTable()) {
		read_gif_color_table(gR, n_bg_colors);
		g_flags.has_bg_fill = true;
	}
	DebugLog("\nSCREEN: %dx%d fill=%d colors=%d\n",
		g_screenWidth, g_screenHeight, g_background , n_bg_colors);
	do {
		switch ((pack.b_size = gR.readUint8())) {
		case ',': // frame
			DebugLog("FRAME:%d\n", idx);
			// read and decode idata blocks for last image on stack
			read_gif_image_data(gR, idx, 0x100);
			idx = addFrame();
			break;
		case '!': // extension
			pack.b_size = gR.readUint8();
			DebugLog("\nEXT@%x\n", pack.b_size);
			// only F9 needs for img, all others moves to stream
			read_gif_image_data(gR, idx, pack.b_size);
			break;
		case ';': // terminator
			pack.b_size = gR.readUint8();
			pack.scan_next = false;
			break;
		case '\0':
			break;
		default:
			if (++unk_block > 20) {
				ecode = eStatus::CorruptedData;
				pack.scan_next = false;
			}
		}
	} while (!gR.isEnd() && pack.scan_next);

	if (g_frames.at(idx).empty())
		delFrame();
	return ecode;
}
