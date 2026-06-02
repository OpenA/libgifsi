#ifndef _GifSi_IO_H_
# include <cstdlib>
# define _GifSi_IO_H_

class DataWriter {

	unsigned char *m_data;
	unsigned int   m_size, m_cap;

public:
	enum {
		InitCapacity = 1024,
		MaxBlockSize = 256
	};
	DataWriter(int nb = 0, int rb = InitCapacity) {
		m_cap  = (m_size = nb) ?: rb;
		m_data = (unsigned char *)std::malloc(m_cap);
	}
	auto data () const -> unsigned char* { return m_data; }
	auto size () const -> unsigned long  { return m_size; }
	void clear() {
		std::free(m_data);
		m_size = m_cap = 0;
		m_data = nullptr;
	}
	void extendCap(int n) {
		int k = 0;
		while ((n + m_size) > (k + m_cap))
			k += MaxBlockSize;
		if (k != 0) {
			m_data = (unsigned char *)std::realloc(m_data, m_cap += k);
		}
	}
	void writeChunk(int n, unsigned char const *buf) {
		 extendCap(n);
		for (int i = 0; i < n; i++) {
			m_data[m_size++] = buf[i];
		}
	}
	void writeUint8 (unsigned char  c) { writeNumber(c); }
	void writeUint16(unsigned short h) { writeNumber(h); }
	void writeUint32(unsigned int   i) { writeNumber(i); }

	template <typename T> void writeNumber(T v) {
		int i = m_size;
		(void)( m_size += sizeof(T) );
		((T *) &m_data[i])[0] = v;
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
	void readChunk(int n, unsigned char *buf) {
		for (int i = 0; i < n; i++)
			buf[i] = m_pos < m_len ? m_data[m_pos++] : 0;
	}
	auto readUint8() -> unsigned char {
		return m_data[m_pos++];
	}
	auto readUint16() -> unsigned short {
		auto h = data<unsigned short>()[0];
		m_pos += sizeof(short);
		return h;
	}
	auto readUint32() -> unsigned int {
		auto i = data<unsigned int>()[0];
		m_pos += sizeof(int);
		return i;
	}
	template <typename T> const T * data() const {
		return (const T *)&m_data[m_pos];
	}
};

# if defined(WITH_FILE_IO) || defined(DEBUG)
# include <cstdio>

class FileReader {

	FILE *m_io;
	bool is_eof;
public:
	FileReader(const char *file) {
		m_io = fopen(file, "rb");
		is_eof = false;
	}
	~FileReader() {
		fclose(m_io);
	}
	bool isEnd() const { return is_eof; }

	void skipBytes(int n) {
		if (fseek(m_io, n, SEEK_CUR))
			is_eof = feof(m_io) == EOF;
	}
	void readChunk(int n, unsigned char buf[]) {
		for (int i = fread(buf, sizeof(char), n, m_io); i < n; i++)
			buf[i] = 0, is_eof = true;
	}
	auto readUint8() -> unsigned char {
		int c = getc(m_io);
		if (c == EOF)
			c = 0, is_eof = true;
		return c;
	}
	auto readUint16() -> unsigned short {
		unsigned short h = 0;
		if (fread(&h, sizeof(short), 1, m_io) != 1)
			is_eof = true;
		return h;
	}
	auto readUint32() -> unsigned int {
		unsigned int i = 0;
		if (fread(&i, sizeof(int), 1, m_io) != 1)
			is_eof = true;
		return i;
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
# ifdef DEBUG
#  define DebugLog(...) fprintf(stderr, __VA_ARGS__)
# else
#  define DebugLog(...)
# endif
#endif //_GifSi_IO_H_
