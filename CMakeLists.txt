cmake_minimum_required(VERSION 3.14)

# Library name and version
project(GifSi
	VERSION 2.0
	LANGUAGES C CXX
)
set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

list(APPEND CMAKE_REQUIRED_LIBRARIES m)

set(PROJECT_LIBS    m)
set(PROJECT_CFLAGS  -O1)
set(PROJECT_DEFS    "")
set(PROJECT_INCLUDE src)
set(PROJECT_SOURCES src/gifsi.hpp src/gifsi.cpp)

add_subdirectory(src)

# Detect CPU type and whether we're building 64-bit or 32-bit code
math(EXPR BITS "${CMAKE_SIZEOF_VOID_P} * 8")
string(TOLOWER ${CMAKE_SYSTEM_PROCESSOR} CPU_TYPE)

message("\nConfigurations:\n")

#
# Optional: -DINSTALL_DIR="/opt/GifSi"
#
option(INSTALL_DIR "Install path" OFF)
if(NOT INSTALL_DIR)
	set(CMAKE_INSTALL_PREFIX /opt/GifSi)
elseif (INSTALL_DIR NOT STREQUAL "")
	set(CMAKE_INSTALL_PREFIX ${INSTALL_DIR})
endif()
message(" Install prefix     = ${CMAKE_INSTALL_PREFIX}")

#
# Optional: -DEXTRA_CFLAGS="-O2"
#
option(EXTRA_CFLAGS "Add compiler flags manually" OFF)
if(NOT EXTRA_CFLAGS AND MSVC)
	set(PROJECT_CFLAGS /O1)
elseif (EXTRA_CFLAGS)
	set(PROJECT_CFLAGS ${EXTRA_CFLAGS})
endif()
message(" Compiler flags     = ${PROJECT_CFLAGS}")

#
# Optional: -DBUILD_SHARED=1
#
option(BUILD_SHARED "Build GifSi as shared library" OFF)
if(NOT BUILD_SHARED)
	add_library(GifSi ${PROJECT_SOURCES})
	message(" Build library type = static")
else()
	add_library(GifSi SHARED ${PROJECT_SOURCES})
	message(" Build library type = shared")
endif()

option(BUILD_TOOL "Build GifSi command line tool" OFF)
if(BUILD_TOOL)
	add_executable(gsitool src/main.cpp)
	message(" Build program name = gsitool")
endif()

if(MSVC)
	# warning level 4 and all warnings as errors
	set(warn_level /W4)
else()
	# lots of warnings and all warnings as errors
	set(warn_level -Wall -Wextra)
endif()
message(" Build warning opts = ${warn_level}")
message(" Build defines list = ${PROJECT_DEFS}\n")

target_include_directories(GifSi SYSTEM AFTER PUBLIC ${PROJECT_INCLUDE})
target_compile_options    (GifSi PRIVATE ${PROJECT_CFLAGS} ${warn_level})
target_compile_definitions(GifSi PRIVATE ${PROJECT_DEFS})
target_link_libraries     (GifSi PRIVATE ${PROJECT_LIBS})
if(BUILD_TOOL)
	target_link_libraries (gsitool PRIVATE GifSi)
endif()
set_target_properties(GifSi PROPERTIES
	MACOSX_BUNDLE_GUI_IDENTIFIER my.example.com
	MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
	MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
)
