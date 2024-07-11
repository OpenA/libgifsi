#ifndef _GifSi_IO_H_
# include <vector>
# define _GifSi_IO_H_

class DataWriter {

	std::vector<unsigned char> m_data;
public:
	DataWriter(unsigned capacity = 1024) {
		m_data.reserve( capacity );
	}
	unsigned char const* data() const { return m_data.data(); }
	unsigned int         size() const { return m_data.size(); }

	void writeChunk(int n, unsigned char const buf[]) {
		for (int i = 0; i < n; i++)
			m_data.push_back(buf[i]);
	}
	void writeUint8 (unsigned char  c) { m_data.push_back(c); }
	void writeUint16(unsigned short h) { writeNumber(h); }
	void writeUint32(unsigned int   i) { writeNumber(i); }

	template <typename T> void writeNumber(T v) {
		unsigned i = m_data.size();
		m_data.resize(i + sizeof(T));
		((T*)&m_data[i])[0] = v;
	}
};

class DataReader {

	unsigned char const *m_data;
	unsigned int  m_pos, m_len;
public:
	DataReader(const unsigned char *data, unsigned len) : m_data(data) {
		m_len = len;
		m_pos = 0;
	}
	DataReader(DataWriter &w) : m_data(w.data()) {
		m_len = w.size();
		m_pos = 0;
	}
	bool isEnd() const { return m_pos >= m_len; }

	void skipBytes(int n) { m_pos += n; }
	void readChunk(int n, unsigned char buf[]) {
		for (int i = 0; i < n; i++)
			buf[i] = m_pos < m_len ? m_data[m_pos++] : 0;
	}
	auto readUint8() -> unsigned char {
		return m_data[m_pos++];
	}
	auto readUint16() -> unsigned short {
		auto h = dataBegin<unsigned short>()[0];
		m_pos += sizeof(short);
		return h;
	}
	auto readUint32() -> unsigned int {
		auto i = dataBegin<unsigned int>()[0];
		m_pos += sizeof(int);
		return i;
	}
	template <typename T> const T *dataBegin() {
		return (const T *)&m_data[m_pos];
	}
	auto dataExtract(int n) -> unsigned char const * {
		auto b = dataBegin<unsigned char>();
		m_pos += n;
		return b;
	}
	void dataRelease(const unsigned char *buf) {
		(void)buf;
	}
};

# ifdef WITH_FILE_IO
#  include <stdio.h>

class FileReader {

	FILE *m_io;
public:
	FileReader(const char *file) {
		m_io = fopen(file, "rb");
	}
	~FileReader() {
		fclose(m_io);
	}
	bool isEnd() const { return feof(m_io) == EOF; }

	void skipBytes(int n) {
		if (n > fseek(m_io, n, SEEK_CUR));
	}
	void readChunk(int n, unsigned char buf[]) {
		for (int i = fread(buf, sizeof(char), n, m_io); i < n; i++)
			buf[i] = 0;
	}
	auto readUint8() -> unsigned char {
		int c = getc(m_io);
		return c == EOF ? 0 : c;
	}
	auto readUint16() -> unsigned short {
		unsigned short h = 0;
		if (!fread(&h, sizeof(short), 1, m_io));
		return h;
	}
	auto readUint32() -> unsigned int {
		unsigned int i = 0;
		if (!fread(&i, sizeof(int), 1, m_io));
		return i;
	}
	auto dataExtract(int n) -> unsigned char* {
		auto buf = new unsigned char[n+1];
		readChunk(n, buf); buf[n] = 0;
		return buf;
	}
	void dataRelease(const unsigned char *buf) {
		delete buf;
	}
};

class FileWriter {

	FILE *m_io;
	unsigned m_pass, m_miss;
public:
	FileWriter(const char *file) {
		m_io = fopen(file, "wb");
	}
	~FileWriter() {
		fclose(m_io);
	}
	unsigned missCount() const { return m_miss; }
	unsigned passBytes() const { return m_pass; }

	void writeChunk(int n, const unsigned char buf[]) {
		int wb  = fwrite(buf, sizeof(char), n, m_io);
		m_miss += wb != n;
		m_pass += wb;
	}
	void writeUint8(unsigned char c) {
		if (putc(c, m_io) == c)
		/**/ m_pass += 1;
		else m_miss ++;
	}
	void writeUint16(unsigned short h)  {
		if (fwrite(&h, sizeof(short), 1, m_io) == 1)
		/**/ m_pass += 2;
		else m_miss ++;
	}
	void writeUint32(unsigned int i) {
		if (fwrite(&i, sizeof(int), 1, m_io) == 1)
		/**/ m_pass += 4;
		else m_miss ++;
	}
};

# endif
#endif //_GifSi_IO_H_
