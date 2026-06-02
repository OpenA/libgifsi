#ifndef _GifSi_H_
# include <vector>
# define _GifSi_H_

namespace GifSi {
	class Image;
	class Stream;
	class Text;
	union Frame;

	enum Consts {
		No_Copy_Colors = 0x1,
		No_Copy_Frames = 0x2,

		Max_Screen_H = (unsigned short)-1,
		Max_Screen_W = (unsigned short)-1,
	};

	enum Disposal {
		Ds_None = 0,
		Ds_Asis,
		Ds_Background,
		Ds_Previous,
		Ds_Unknown
	};

	union Color {
		struct { unsigned char r,g,b,a; };
		struct { unsigned int  value:24, flags:8; };
	};
	struct Rect {
		  signed short x,y;
		unsigned short w,h;
	};
	// error code and warn level
	enum eStatus {
		EvrethingOK,
	// Error:
		UnknownStream,
		NotSupported,
		CorruptedData
	};
}

class GifSi::Text {
// null-terminated text
	const char *m_text;
// bounds rect
	struct Rect m_rect;
// text measurments
	unsigned char m_lineH, m_strokeColor, m_fillColor, m_lSpace;
	unsigned char m_charW, m_strokeWidth, m_bgColor;
// font style
	struct __attribute__((packed)) {
		bool italic:1, bold:1, underline:1;
		bool strike:1, caps:1, overline :1;
	} m_style;
public:
	Text() {}

	friend Stream;
};

class GifSi::Image {

	unsigned char *m_pixels;
// 8-byte bounds
	struct Rect m_rect;
// 8-byte props
	unsigned short m_delay, m_sic;
	unsigned char  m_alpha, m_eic, m_bpp;
// external data
	struct __attribute__((packed)) {

		enum Disposal disposal:3;

		bool interlace:1, transparent:1;
	} m_prop;
public:

	Image() {}
	Image(const Image& src, bool empty) {
		m_rect  = src.m_rect;
		m_alpha = src.m_alpha;
		m_delay = src.m_delay;
		m_prop  = src.m_prop;
		m_bpp   = src.m_bpp;

		if (!empty && src.m_pixels) {
			//m_pixels = (unsigned char *)std::malloc(size());

			for (int i = 0; i < size(); i++)
				m_pixels[i] = src.m_pixels[i];
		}
	}
	int bpp   () const { return m_bpp; }
	int size  () const { return width() * height(); }
	int left  () const { return m_rect.x; }
	int top   () const { return m_rect.y; }
	int width () const { return m_rect.w; }
	int height() const { return m_rect.h; }

	int checkBounds() {
		int w = m_rect.w, x = m_rect.x;
		int h = m_rect.h, y = m_rect.y;
		// If still zero, error.
		if (w == 0 || h == 0)
			return 201;
		// If position out of range, error.
		if ((x + w) > Max_Screen_W || (y + h) > Max_Screen_H)
			return 202;
		return 0;
	}

	unsigned char &operator[](int i) {
		return m_pixels[i];
	}
protected:
	friend Stream;
};

union GifSi::Frame {

	Image image;
	Text  text;

	enum Type {
		TypeNone, TypeImage, TypeText, TypeMeta
	};
	struct __attribute__((packed)) Self {
		void *_ptr;

		struct Rect _r;

		unsigned int _0,_1,_2,_3:24;

		enum Type _typ:8;
	} self;
	
	Frame() {
		self._typ = TypeNone;
		self._ptr = nullptr;
	}
	~Frame();
	void setup(enum Type, unsigned int);
	Type type () const { return self._typ; }
	bool empty() const { return !self._ptr; }
};

class GifSi::Stream {

	std::vector<Frame> g_frames;
	std::vector<Color> g_colors;

	unsigned short g_screenWidth, g_screenHeight;
	unsigned short g_loopsCount;
	unsigned char  g_background;

	struct __attribute__((packed)) Flags {
		bool has_limit_loops :1, no_metadata:1;
		bool has_local_colors:1, has_bg_fill:1;
	} g_flags;

public:


	Stream() {
		g_colors.reserve(255);
		g_frames.reserve(100);
		g_flags = {false,false,false,false};
	}
	Stream(const Stream& other, enum Consts fl) : Stream() {
		g_screenWidth  = other.g_screenWidth;
		g_screenHeight = other.g_screenHeight;
		g_loopsCount   = other.g_loopsCount;
		g_background   = other.g_background;
		g_flags        = other.g_flags;

		if (!(fl & No_Copy_Colors)) {
			g_colors = other.g_colors;
		}
		if (!(fl & No_Copy_Frames)) {
			g_frames = other.g_frames;
		}
	}
	~Stream(){};

	auto  addColor(Color) -> int;
	auto findColor(Color, int sidx = 0, int cn = 0) -> int;

	void delFramesFrom(int sidx, int n = 1);
	void addFramesTo  (int sidx, int n = 1);
	bool hasFrameEmpty(int sidx) {
		return !g_frames.at(sidx).self._ptr;
	}

	void delFrame() { g_frames.pop_back(); }
	auto addFrame() -> int {
		int i = g_frames.size();
		/*---*/ g_frames.resize(i + 1);
		return  i;
	}

	int loopsCount  () const { return g_loopsCount;    }
	int framesCount () const { return g_frames.size(); }
	int screenWidth () const { return g_screenWidth;   }
	int screenHeight() const { return g_screenHeight;  }

	void setLoopCount(unsigned short l) { g_loopsCount = l; }
	void resizeScreen(unsigned short w, unsigned short h) {
		g_screenWidth  = w,
		g_screenHeight = h;
	}
	auto read(const unsigned char *data, const int len) -> eStatus;
	auto read(const char *file) -> eStatus;

protected:
	template<class T> auto read_magic_number(T&) -> eStatus;
#ifdef WITH_GIF
	template<class T> auto read_gif_stream(T&) -> eStatus;
	template<class T> void read_gif_color_table(T&, int);
	template<class T> void read_gif_image_data (T&, int, unsigned);
#endif
};

#endif // _GifSi_H_
