
#include "gifsi.hpp"
#include "gifsi_io.hpp"

#ifdef WITH_GIF
# include "gif_format.cpp"
#endif

using namespace GifSi;

void Frame::setup(enum Type _t, unsigned int _sz) {
	self._ptr = _sz ? std::malloc(_sz) : self._ptr;
	self._typ = _t;
}
Frame::~Frame() {
	bool sucess = false;
	if ((sucess = !empty()))
		std::free(self._ptr), self._ptr = nullptr;
	DebugLog("END :: %d,%d ::\n", type(), sucess);
}

inline auto Stream::findColor(Color col, int sidx/*= 0*/, int cn/*= 0*/) -> int {
	auto sp = g_colors.begin() + sidx;
	auto ep = g_colors.end();

	ep = (cn > 0 ? sp : ep) + cn;

	for (auto c = col.value; sp < ep; sidx++, sp++) {
		if (c == sp[0].value)
			return sidx;
	}
	return -1;
}

inline auto Stream::addColor(Color col) -> int {
	int idx = findColor(col);
	if (idx == -1) {
		idx = g_colors.size();
		/***/ g_colors.push_back(col);
	}
	return idx;
}

inline void Stream::delFramesFrom(int sidx, int n/*= 1*/) {
	auto  sp = g_frames.begin();
	int   sz = g_frames.size();
	if (sidx < 0)
		sidx = sz + sidx;
	g_frames.erase(sp + sidx, sp + (sidx+n));
}

inline void Stream::addFramesTo(int sidx, int n/*= 1*/)  {
	auto  nf = std::vector<Frame>(n);
	auto  sp = g_frames.begin();
	int   sz = g_frames.size();
	if (sidx < 0)
		sidx = sz + sidx;
	g_frames.insert(sp + sidx, nf.begin(), nf.end());
}

template<class T>
inline auto Stream::read_magic_number(T &gR) -> eStatus
{
	auto ok = eStatus::EvrethingOK;
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
		ok = read_gif_stream(gR);
# else
		ok = eStatus::NotSupported;
# endif
	} else {
		ok = eStatus::UnknownStream;
	}
	return ok;
}

auto Stream::read(const unsigned char *data, const int len) -> eStatus {
	DataReader gR(data, len);
	return read_magic_number(gR);
}

auto Stream::read(const char *file) -> eStatus {
#ifdef WITH_FILE_IO
	FileReader gR(file);
	return read_magic_number(gR);
#else
	return eStatus::NotSupported;
#endif
}
