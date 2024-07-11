#ifndef _GifSi_H_
# include <vector>
# define _GifSi_H_

namespace GifSi {
	class Image;
	class Stream;
	class Colormap;

	enum Flags {
		Exclude_Colormap   = 0x1,
		Exclude_Extensions = 0x2,
		Exclude_Images     = 0x8,

		PNG_Source = 0x100,
		Gif_Source = 0x200,

		Max_ScreenWidth  = 0xFFFFu,
		Max_ScreenHeight = 0xFFFFu,
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
	// external data types
	enum exType {
		Unknown = 0,
		AppExtend,
		GfxControl,
		Identifer,
		Comment,
		iData,
	};
	// structured external data
	struct exDat {
		unsigned char const *data;
		unsigned int type:8, imdx:24, size;
	};
	// error code and warn level
	enum eLevel {
		OK = 0, Warning, Error, Fatal
	};
	struct eCode {
		enum eLevel wlvl:6;
		unsigned code:10, wcnt:16;
	};
	typedef std::vector<Image> ImageList;
	typedef std::vector<exDat> ExdatList;
}

class GifSi::Colormap {
	std::vector<Color> m_map;
public:
	int indexOf(const Color, int sidx = 0);
	int add(Color);
	int count() { return m_map.size(); }

# ifdef WITH_GIF
	template<typename T> void read_color_table(T&, int ncol);
# endif
};

class GifSi::Image {

	  signed short m_left,  m_top;
	unsigned short m_width, m_height;

	unsigned char *m_pixels, m_bpp;

	unsigned short m_delay;
	unsigned char  m_alpha, m_disposal:3, m_cpyflags:3;

	bool has_interlace:1, has_transparent:1;

	Colormap  m_colors;
	ExdatList m_raw;
public:

	Image() {}
	Image(const Image& src, unsigned exclude_flags) {
		m_left   = src.m_left;
		m_top    = src.m_top;
		m_width  = src.m_width;
		m_height = src.m_height;

		m_alpha = src.m_alpha;
		m_delay = src.m_delay;

		m_disposal = src.m_disposal;
		m_cpyflags = exclude_flags;

		has_interlace   = src.has_interlace;
		has_transparent = src.has_transparent;

		if (!(exclude_flags & Exclude_Colormap)) {
			m_colors = src.m_colors;
		}
		if (!(exclude_flags & Exclude_Extensions)) {
			m_raw = src.m_raw;
		}
	}
	~Image() {
		delete m_pixels;
	}

	void operator=(const Image& src) {
		Image(src, src.m_cpyflags);
	}
	bool hasLocalColors() {
		return !!m_colors.count();
	}
	bool hasEmpty() {
		return !m_raw.size() && !m_pixels;
	}
	int checkBounds() {
		// If still zero, error.
		if (m_width == 0 || m_height == 0)
			return 201;
		// If position out of range, error.
		if ((m_left + m_width) > Max_ScreenWidth || (m_top + m_height) > Max_ScreenHeight)
			return 202;
		return 0;
	}

protected:
	friend Stream;
#ifdef WITH_GIF
	template<typename T> void read_gif_image_data(T&, Stream*, exType);
	template<typename T> void decode_gif_image();
#endif
};

class GifSi::Stream {

	unsigned short m_screenWidth, m_screenHeight;
	unsigned short m_loopcount;
	unsigned char  m_background, m_cpyflags:5;
	
	bool has_limit_loops :1, has_bg_color :1,
	     has_local_colors:1;

	Colormap  g_colors;
	ImageList m_images;
public:

	Stream() {}
	Stream(const Stream& other, unsigned no_copy_flags) {
		m_screenWidth  = other.m_screenWidth;
		m_screenHeight = other.m_screenHeight;
		m_loopcount    = other.m_loopcount;
		m_background   = other.m_background;
		m_cpyflags     = no_copy_flags;

		has_bg_color     = other.has_bg_color;
		has_limit_loops  = other.has_limit_loops;
		has_local_colors = false;

		if (!(no_copy_flags & Exclude_Colormap)) {
			g_colors = other.g_colors;
		}
		if (!(no_copy_flags & Exclude_Images)) {
			m_images = other.m_images;
			has_local_colors = other.has_local_colors;
		}
	}
	~Stream() {}

	void operator=(const Stream& other) {
		Stream(other, other.m_cpyflags);
	}

	void delImagesFrom(int sidx, int n = 1);
	void delImage() { m_images.pop_back(); };
	auto addImage() -> int {
		int i = m_images.size();
		m_images.resize(i + 1);
		return i;
	};

	auto screenWidth () -> unsigned short { return m_screenWidth;  }
	auto screenHeight() -> unsigned short { return m_screenHeight; }

	void setLoopCount(unsigned short l) { m_loopcount = l; }
	void resizeScreen(unsigned short w, unsigned short h) {
		m_screenWidth  = w,
		m_screenHeight = h;
	}
	auto read(const unsigned char *data, const int len) -> eCode;
	auto read(const char *file) -> eCode;

protected:
	template<typename T> auto read_magic_number(T&) -> eCode;
#ifdef WITH_GIF
	template<typename T> auto read_gif_stream(T&) -> int;
#endif
};

#endif // _GifSi_H_
