
set(project_defs "")
set(project_src
	src/gifsi_io.hpp
)

option(WITH_GIF "enable GIF Optim module" ON)
if (WITH_GIF)
	set(project_defs ${project_defs} -DWITH_GIF)
endif()

option(WITH_FILE_IO "enable Read/Write from files" ON)
if (WITH_FILE_IO)
	set(project_defs ${project_defs} -DWITH_FILE_IO)
endif()

option(WITH_SIMD "enable SIMD intrinsics" ON)
if (WITH_SIMD)
	set(project_defs ${project_defs} -DWITH_SIMD)
endif()

option(WITH_OMP "enable OpenMP parallel processing" OFF)
if (WITH_OMP)
	set(project_defs ${project_defs} -DWITH_OMP)
endif()

option(DEBUG_MODE "enable Debug mode" OFF)
if (DEBUG_MODE)
	set(project_defs ${project_defs} -DDEBUG)
endif()

set(PROJECT_DEFS    ${PROJECT_DEFS}    ${project_defs} PARENT_SCOPE)
set(PROJECT_INCLUDE ${PROJECT_INCLUDE} ${include_dirs} PARENT_SCOPE)
set(PROJECT_SOURCES ${PROJECT_SOURCES} ${project_src}  PARENT_SCOPE)
