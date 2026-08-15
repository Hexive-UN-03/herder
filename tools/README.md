# herder helper programs

The three programs herder shells out to for anything that touches a VCF. They're here so
the package doesn't ship 21 MB of binaries with no source, and so somebody other than the
original author can rebuild them.

| program | invoked as | called from |
|---|---|---|
| `sample_lister` | `sample_lister <vcf> <outpath>` | `R/preprocessing.R` |
| `fast_af` | `fast_af <vcf> <region> <threads> <samplefile> <outpath>` | `R/app_server.R` |
| `vcf_trimmer` | `vcf_trimmer <vcf> <samplefile> <outpath> [region]` | `R/app_server.R` |

They're linked static on purpose: the point of having them is that a user doesn't need
bcftools installed or a matching libstdc++.

R locates them by name through `herder_bin()` in `R/utils_helpers.R`, which looks in
`inst/scripts/` and adds `.exe` on Windows. **Don't rename the targets.**

## Building natively

```sh
cd tools
cmake --preset native
cmake --build build/native -j8
cmake --install build/native      # copies them into ../inst/scripts
```

Needs cmake ≥ 3.19, autotools (htslib configures with them), and zlib/bzip2/lzma headers.
htslib and EAGLE are fetched from git at configure time.

## Building for Windows

```sh
cd tools
cmake --preset windows
cmake --build build/windows -j8
```

You need an `x86_64-w64-mingw32` toolchain on `PATH`. If you haven't got one, build the
container:

```sh
export APPTAINER_TMPDIR=$HOME/opt/apptmp APPTAINER_CACHEDIR=$HOME/.apptainer/cache
apptainer build --fakeroot xbuild.sif xbuild.def
apptainer exec xbuild.sif bash -c 'cmake --preset windows && cmake --build build/windows -j8'
```

Check what came out:

```sh
file build/windows/fast_af.exe
# -> PE32+ executable (console) x86-64, for MS Windows
x86_64-w64-mingw32-objdump -p build/windows/fast_af.exe | grep 'DLL Name'
# -> KERNEL32.dll, WS2_32.dll, msvcrt.dll  (system only; nothing to ship alongside)
```

### Three things that will bite you

1. **Use the `-posix` compilers.** Ubuntu's mingw-w64 defaults to the win32 threading
   model, which has no `<thread>`/`<future>`. `std::future` shows up as an incomplete
   type and `fast_af.cpp` won't compile. The toolchain file pins
   `x86_64-w64-mingw32-g++-posix` for this reason.

2. **htslib needs POSIX `<regex.h>`, mingw hasn't got one.** `hts_expr.c` includes it
   unconditionally, so the build dies there. We build TRE (the same regex implementation
   MSYS2 uses behind libsystre) and drop a one-line `regex.h` that forwards to
   `<tre/regex.h>`.

3. **Don't use absolute library paths in `find_library`.** The previous build files did
   `PATHS /usr/lib/x86_64-linux-gnu ... NO_DEFAULT_PATH`, and `NO_DEFAULT_PATH` skips
   `CMAKE_FIND_ROOT_PATH` — so the "windows" build quietly linked host Linux `.a` files
   and never produced a Windows binary at all. Check
   `build/windows/CMakeCache.txt` for any `/usr/lib/x86_64-linux-gnu` if something looks
   wrong.

Also note bz2, lzma, libcurl and plugins are all switched off for the Windows build.
That means no CRAM and no reading VCFs over http/s3 on Windows. Neither matters here —
herder only ever opens a local bgzipped, tabix-indexed VCF.
