
#include "gifsi.hpp"
#include "gifsi_io.hpp"

#ifdef WITH_GIF
# include "gif_format.cpp"
#endif

using namespace GifSi;

inline auto Colormap::indexOf(const Color col, int sidx/* = 0*/) -> int {
	const auto c = col.value;
	for ( auto a = m_map.begin()+sidx; a < m_map.end(); sidx++, a++ ) {
		if (c == a[0].value)
			return sidx;
	}
	return -1;
}

inline auto Colormap::add(Color col) -> int {
	int idx = indexOf(col);
	if (idx == -1) {
		idx = m_map.size();
		m_map.push_back(col);
	}
	return idx;
}

inline void Stream::delImagesFrom(int sidx, int n/* = 1*/) {
	if (sidx < 0)
		sidx = m_images.size() + sidx;
	m_images.erase(m_images.begin() + sidx, m_images.begin() + (sidx+n));
};

template<typename T>
inline auto Stream::read_magic_number(T &gR) -> eCode
{
	eCode ok = {
		.wlvl = eLevel::OK,
		.code = 0,
		.wcnt = 0
	};
	unsigned char magic[4];
	// read magic number
	for (int i = 0; i < 4; i++)
		magic[i] = gR.readUint8();
	if (
		magic[0] == 'G' &&
		magic[1] == 'I' &&
		magic[2] == 'F'
	) { 
# ifdef WITH_GIF
		gR.skipBytes(2); // GIF8 + 9a
		ok.code = read_gif_stream(gR);
# else
		ok.wlvl = eLevel::Error;
		ok.code = 101; // gif not supported
# endif
	} else {
		ok.wlvl = eLevel::Error;
		ok.code = 100; // unknown stream
	}
	return ok;
}

auto Stream::read(const unsigned char *data, const int len) -> eCode {
	DataReader gR(data, len);
	return read_magic_number(gR);
}

auto Stream::read(const char *file) -> eCode {
#ifdef WITH_FILE_IO
	FileReader gR(file);
	return read_magic_number(gR);
#endif
}
