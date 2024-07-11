#include "gifsi.hpp"
#include <string>

int main(int argc, const char *argv[]) {
	auto stm = GifSi::Stream();
	if (argv[1])
		stm.read(argv[1]);
	return 0;
}