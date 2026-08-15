# Build the three helper programs herder shells out to.
#
# One project with three targets instead of three near-identical projects, because the
# three copies had already drifted apart from each other.
#
#   native:   cmake --preset native  && cmake --build build/native
#   windows:  cmake --preset windows && cmake --build build/windows
#
# See README.md. The windows preset needs a mingw-w64 cross toolchain; there's an
# apptainer recipe in this directory for getting one on a cluster that hasn't got it.

cmake_minimum_required(VERSION 3.19)
project(herder_tools VERSION 0.1.0 LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(BUILD_SHARED_LIBS OFF CACHE BOOL "" FORCE)

include(FetchContent)
include(ExternalProject)

# NOTE: the old build files did
#     set(CMAKE_PREFIX_PATH "/usr/lib/x86_64-linux-gnu")
#     find_library(... PATHS /usr/lib /usr/lib/x86_64-linux-gnu NO_DEFAULT_PATH)
# which is why the "windows" build never actually produced a windows binary: NO_DEFAULT_PATH
# skips CMAKE_FIND_ROOT_PATH, so it happily linked host Linux .a files into a PE target.
# Don't reintroduce absolute library paths here -- let the toolchain file decide.

# EAGLE: header-only, gives us ordered_parallel_output.hpp
FetchContent_Declare(EAGLE
  GIT_REPOSITORY https://github.com/Hexive-UN-03/EAGLE.git
  GIT_TAG        main)
FetchContent_MakeAvailable(EAGLE)

set(DEPS_PREFIX ${CMAKE_BINARY_DIR}/deps)

if(CMAKE_CROSSCOMPILING)
  # ---------------------------------------------------------------------------
  # Windows. Nothing useful is in the mingw sysroot, so everything gets built here.
  # ---------------------------------------------------------------------------
  set(CROSS_HOST x86_64-w64-mingw32)

  # zlib. htslib will not configure without it.
  ExternalProject_Add(zlib_ext
    GIT_REPOSITORY https://github.com/madler/zlib.git
    GIT_TAG        v1.3.1
    CMAKE_ARGS
      -DCMAKE_TOOLCHAIN_FILE=${CMAKE_TOOLCHAIN_FILE}
      -DCMAKE_INSTALL_PREFIX=${DEPS_PREFIX}
      -DCMAKE_BUILD_TYPE=Release
    # zlib's cmake installs the static lib as libzlibstatic.a, but htslib's configure
    # looks for -lz, so give it the name it expects
    INSTALL_COMMAND ${CMAKE_COMMAND} --build . --target install
            COMMAND ${CMAKE_COMMAND} -E copy ${DEPS_PREFIX}/lib/libzlibstatic.a
                                             ${DEPS_PREFIX}/lib/libz.a)

  # TRE. mingw has no POSIX <regex.h> and htslib's hts_expr.c includes it unconditionally,
  # which is the wall everyone hits trying to build htslib for windows. TRE is the same
  # implementation MSYS2 uses for its libsystre.
  ExternalProject_Add(tre_ext
    GIT_REPOSITORY https://github.com/laurikari/tre.git
    GIT_TAG        master
    CONFIGURE_COMMAND ./utils/autogen.sh
              COMMAND ./configure --host=${CROSS_HOST}
                        CC=${CMAKE_C_COMPILER}
                        AR=${CMAKE_AR} RANLIB=${CMAKE_RANLIB}
                        --prefix=${DEPS_PREFIX}
                        --disable-shared --enable-static --disable-agrep --disable-nls
    BUILD_COMMAND     make -j
    INSTALL_COMMAND   make install
            # TRE installs its header as tre/regex.h; drop a plain regex.h next to it so
            # htslib's #include <regex.h> resolves
            COMMAND ${CMAKE_COMMAND} -E echo "#include <tre/regex.h>"
                    > ${DEPS_PREFIX}/include/regex.h
    BUILD_IN_SOURCE 1)

  ExternalProject_Add(htslib_ext
    GIT_REPOSITORY https://github.com/samtools/htslib.git
    GIT_TAG        1.24
    GIT_SUBMODULES_RECURSE ON
    DEPENDS zlib_ext tre_ext
    CONFIGURE_COMMAND autoreconf -i
              COMMAND ./configure --host=${CROSS_HOST}
                        CC=${CMAKE_C_COMPILER}
                        AR=${CMAKE_AR} RANLIB=${CMAKE_RANLIB}
                        CPPFLAGS=-I${DEPS_PREFIX}/include
                        LDFLAGS=-L${DEPS_PREFIX}/lib
                        LIBS=-ltre
                        # none of these are available for mingw and we don't read
                        # cram/remote files, so switch them off rather than porting them
                        --disable-bz2 --disable-lzma --disable-libcurl --disable-plugins
                        --without-libdeflate --disable-shared --enable-static
    BUILD_COMMAND     make -j lib-static
    INSTALL_COMMAND   ""
    BUILD_IN_SOURCE 1)

  ExternalProject_Get_Property(htslib_ext SOURCE_DIR)
  set(HTS_INCLUDE_DIR ${SOURCE_DIR})
  set(HTS_LIBRARY     ${SOURCE_DIR}/libhts.a)
  set(EXTRA_LIBS
      ${DEPS_PREFIX}/lib/libz.a
      ${DEPS_PREFIX}/lib/libtre.a
      ws2_32 pthread)
  set(DEP_TARGETS htslib_ext)

else()
  # ---------------------------------------------------------------------------
  # Native (Linux/macOS). Use whatever the system provides.
  # ---------------------------------------------------------------------------
  find_package(ZLIB REQUIRED)
  find_package(BZip2 REQUIRED)
  find_package(LibLZMA REQUIRED)

  ExternalProject_Add(htslib_ext
    GIT_REPOSITORY https://github.com/samtools/htslib.git
    GIT_TAG        1.24
    GIT_SUBMODULES_RECURSE ON
    CONFIGURE_COMMAND autoreconf -i
              COMMAND ./configure --disable-libcurl --disable-s3
                                  --disable-shared --enable-static
    BUILD_COMMAND     make -j lib-static
    INSTALL_COMMAND   ""
    BUILD_IN_SOURCE 1)

  ExternalProject_Get_Property(htslib_ext SOURCE_DIR)
  set(HTS_INCLUDE_DIR ${SOURCE_DIR})
  set(HTS_LIBRARY     ${SOURCE_DIR}/libhts.a)
  set(EXTRA_LIBS ZLIB::ZLIB BZip2::BZip2 LibLZMA::LibLZMA pthread)
  set(DEP_TARGETS htslib_ext)
endif()

# the executables themselves. R finds these by name via herder_bin() in R/utils_helpers.R,
# so the target names have to stay exactly these three
foreach(prog fast_af sample_lister vcf_trimmer)
  add_executable(${prog} src/${prog}.cpp)
  add_dependencies(${prog} ${DEP_TARGETS})
  target_include_directories(${prog} PRIVATE ${HTS_INCLUDE_DIR} ${DEPS_PREFIX}/include)
  target_link_libraries(${prog} PRIVATE ${HTS_LIBRARY} EAGLE::EAGLE ${EXTRA_LIBS})
  # ship them standalone -- the whole point of these is that a user doesn't have to
  # install bcftools or match a libstdc++ version
  target_link_options(${prog} PRIVATE -static -static-libgcc -static-libstdc++)
  # and strip them. these get committed to the repo, and the debug info is most of the
  # file: 9.6MB down to 2.0MB each. build with -DCMAKE_BUILD_TYPE=Debug if you need it back
  target_link_options(${prog} PRIVATE $<$<CONFIG:Release>:-s>)
endforeach()

# drop the built programs straight into inst/scripts, which is where herder_bin() looks
install(TARGETS fast_af sample_lister vcf_trimmer
        RUNTIME DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/../inst/scripts)
