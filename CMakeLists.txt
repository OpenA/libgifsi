cmake_minimum_required(VERSION 3.0.2)

# Library name
project(GifSI C CXX)

# Library version
set(VERSION_MAJOR 1)
set(VERSION_MINOR 0)
set(VERSION_PATCH 0)
set(VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH})

list(APPEND CMAKE_REQUIRED_LIBRARIES m)

if(POLICY CMP0075)
  cmake_policy(SET CMP0075 NEW)
endif()

include(GNUInstallDirs)
include(CheckCSourceCompiles)
include(CheckFunctionExists)
include(CheckIncludeFile)
include(CheckTypeSize)

include_directories(AFTER SYSTEM
  ${CMAKE_SOURCE_DIR}/include
  ${CMAKE_BINARY_DIR}
)

# Detect CPU type and whether we're building 64-bit or 32-bit code
math(EXPR BITS "${CMAKE_SIZEOF_VOID_P} * 8")
string(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} CPU_TYPE)

message("\nConfigurations:\n")

#
# Optional: -DCMAKE_INSTALL_PREFIX="/install/path"
#
message(" Install prefix     = ${CMAKE_INSTALL_PREFIX}")

#
# Optional: -DCFLAGS="-O2 -I/path/include"
#
#option(CFLAGS "Add compiler flags manually" OFF)
#if(NOT CFLAGS STREQUAL "")
#  message(" Compiler flags       = ${CFLAGS}")
#else()
#  message(" Compiler flags       = -O2 -Wall")
#endif()

#
# Optional: -DENABLE_SHARED=1
#
option(ENABLE_SHARED "Build GifSI with shared libraries" OFF)
if(NOT ENABLE_SHARED)
  set(gifsi_library_type STATIC)
  message(" Build library type = static")
else()
  set(gifsi_library_type SHARED)
  message(" Build library type = shared")
endif()

#
# libgifsi
#
add_library(libgifsi ${gifsi_library_type}
  src/giffunc.c
  src/gifread.c
  src/gifunopt.c
  src/gifwrite.c
)
set_target_properties(libgifsi PROPERTIES
  OUTPUT_NAME gifsi
  VERSION ${VERSION}
  SOVERSION ${VERSION_MAJOR}
)
install(TARGETS libgifsi
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
#install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/include/gifsi.h
#  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
#)
if(UNIX AND NOT (BEOS OR HAIKU))
  target_link_libraries(libgifsi m)
endif()

#
# Optional: -DBUILD_GIFSICLE=1
#
option(BUILD_GIFSICLE "Build gifsicle program." ON)

if(BUILD_GIFSICLE)
  message(" Build program      = gifsicle")
  add_executable(gifsicle
    src/clp.c
    src/gifsicle.c
    src/merge.c
    src/optimize.c
    src/quantize.c
    src/support.c
    src/xform.c
  )
  target_link_libraries(gifsicle libgifsi)

  install(TARGETS gifsicle
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/gifsicle.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
  )
endif()
unset(BUILD_GIFSICLE)

#
# Optional: -DBUILD_GIFDIFF=1
#
option(BUILD_GIFDIFF "Build gifdiff program." OFF)

if(BUILD_GIFDIFF)
  message(" Build program      = gifdiff")
  add_executable(gifdiff
    src/clp.c
    src/gifdiff.c
  )
  target_link_libraries(gifdiff libgifsi)

  install(TARGETS gifdiff
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/gifdiff.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
  )
endif()
unset(BUILD_GIFDIFF)

#
# Optional: -DBUILD_GIFVIEW=1
#
option(BUILD_GIFVIEW "Build gifview program." OFF)

if(BUILD_GIFVIEW)
  message(" Build program      = gifview")
  add_executable(gifview
    src/clp.c
    src/gifx.c
    src/gifview.c
  )
  target_link_libraries(gifview libgifsi)

  install(TARGETS gifview
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
  install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/gifview.1
    DESTINATION ${CMAKE_INSTALL_MANDIR}/man1
  )
  if(UNIX AND NOT (BEOS OR HAIKU))
    target_link_libraries(gifview X11)
  endif()
endif()
unset(BUILD_GIFVIEW)

#
# Optional: -DWITH_MULTITHREAD=1
#
option(WITH_MULTITHREAD "Enable multithreading support if available." OFF)

if(WITH_MULTITHREAD)
  if(UNIX)
    message(" Threads library    = pthread")
    check_include_file("pthread.h" HAVE_PTHREAD_H)
    if(HAVE_PTHREAD_H)
      set(ENABLE_THREADS 1)
      list(APPEND CMAKE_REQUIRED_FLAGS "-pthread")

      check_c_source_compiles("
#include <pthread.h>
#include <stdio.h>

static volatile int I = 0;

void *func (void *x) {
  __sync_add_and_fetch(&I, 1);
  return x;
}

int main(void) {
  pthread_t t;
  pthread_create(&t, NULL, func, NULL);
  pthread_join(t, NULL);
  printf(\"val = %i\", I);
  return I;
}" PT_FUNC__sync_add_and_fetch)

      set(HAVE___SYNC_ADD_AND_FETCH ${PT_FUNC__sync_add_and_fetch})
      unset(PT_FUNC__sync_add_and_fetch)
    endif()
  endif()
endif()
unset(WITH_MULTITHREAD)

#
# Optional: -DWITH_SIMD=0
#
option(WITH_SIMD
  "Include SIMD extensions, if available for this platform."
  ON
)
if(WITH_SIMD)
  message(" SIMD extensions for ${CPU_TYPE}")

  set(test_vec_code "
#include <stdlib.h>
#include <stdio.h>

typedef float float4 __attribute__((VEC_SIZE));

int main() {
  float4 a;
  for (int i = 0; i <= 4; i++)
    a[i] = (float)rand();
  a = _SHUFFLE(a,a,3,2,1,0);
  printf(\"vec = %f\", a[0]);
  return (int)a[0];
}")
  # check `vector_size` type
  check_c_source_compiles("
#define VEC_SIZE vector_size(4 * sizeof(float))
#define _SHUFFLE(a,b,i3,i2,i1,i0) (a * (float)3)
${test_vec_code}" CC_TYPE_vector_size)

  # check `ext_vector_type` type
  check_c_source_compiles("
#define VEC_SIZE ext_vector_type(4)
#define _SHUFFLE(a,b,i3,i2,i1,i0) (a * (float)3)
${test_vec_code}" CC_TYPE_ext_vector_type)

  # check `__builtin_shufflevector` function
  check_c_source_compiles("
#define VEC_SIZE ext_vector_type(4)
#define _SHUFFLE __builtin_shufflevector
${test_vec_code}" CC_FUNC_builtin_shufflevector)

  set(HAVE_SIMD                         TRUE)
  set(HAVE_VECTOR_SIZE_VECTOR_TYPES     ${CC_TYPE_vector_size})
  set(HAVE_EXT_VECTOR_TYPE_VECTOR_TYPES ${CC_TYPE_ext_vector_type})
  set(HAVE___BUILTIN_SHUFFLEVECTOR      ${CC_FUNC_builtin_shufflevector})

  unset(test_vec_code)
  unset(CC_TYPE_vector_size)
  unset(CC_TYPE_ext_vector_type)
  unset(CC_FUNC_builtin_shufflevector)
endif()
unset(WITH_SIMD)

message("")

#
# Compiler settings
#
add_compile_definitions(HAVE_CONFIG_H=1)

if(MSVC)
  target_compile_definitions(gifsicle PRIVATE _CRT_SECURE_NO_WARNINGS)
  # warning level 4 and all warnings as errors
  add_compile_options(/O2 /Wall)
  # MSVC does not export symbols by default when building a DLL, this is a
  # workaround for recent versions of CMake
  if(CMAKE_VERSION VERSION_LESS 3.4)
    message(WARNING "Automatic export of all symbols to DLL not supported until CMake 3.4")
  elseif(ENABLE_SHARED)
    set_target_properties(libgifsi PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS ON)
  endif()
else()
  # lots of warnings and all warnings as errors
  add_compile_options(-O2 -Wall)
endif()

# check headers
check_include_file("string.h" HAVE_STRING_H )
check_include_file("time.h"   HAVE_TIME_H   )
check_include_file("math.h"   HAVE_MATH_H   )

# check functions
check_function_exists("strerror" HAVE_STRERROR )
check_function_exists("strtoul"  HAVE_STRTOUL  )
check_function_exists("mkstemp"  HAVE_MKSTEMP  )
check_function_exists("pow"      HAVE_POW      )

# check types
check_type_size("uintptr_t" SIZEOF_UINTPTR_T )
check_type_size("uint64_t"  SIZEOF_UINT64_T  )
check_type_size("int64_t"   SIZEOF_INT64_T   )

if(NOT SIZEOF_UINTPTR_T STREQUAL "")
  set(HAVE_UINTPTR_T TRUE)
endif()

if(NOT SIZEOF_UINT64_T STREQUAL "")
  set(HAVE_UINT64_T TRUE)
endif()

if(NOT SIZEOF_INT64_T STREQUAL "")
  set(HAVE_INT64_T TRUE)
endif()

# Finally generate config.h files
if(UNIX)
  check_include_file("sys/types.h" HAVE_SYS_TYPES_H )
  check_include_file("sys/stat.h"  HAVE_SYS_STAT_H  )
  check_include_file("inttypes.h"  HAVE_INTTYPES_H  )
  check_include_file("unistd.h"    HAVE_UNISTD_H    )
  check_include_file("stddef.h"    HAVE_STDDEF_H    )
  configure_file(ConfigUnix.h.in config.h)
elseif(WIN32)
  configure_file(ConfigWin.h.in config.h)
endif()

#message("
#  FLAGS       : ${CMAKE_REQUIRED_FLAGS}
#  DEFINITIONS : ${CMAKE_REQUIRED_DEFINITIONS}
#  INCLUDES    : ${CMAKE_REQUIRED_INCLUDES}
#  LIBRARIES   : ${CMAKE_REQUIRED_LIBRARIES}
#  QUIET       : ${CMAKE_REQUIRED_QUIET}
#")
