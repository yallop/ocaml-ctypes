#!/usr/bin/env bash
set -ex

WODI_ARCH=$1

type -p ocamlc
ocamlc -version

build_libffi=0
libffi_version=3.1

x="$(echo 'let () = print_int Sys.word_size ;;' | ocaml -stdin)"
case "$x" in
    *64*)
        build=x86_64-pc-cygwin
        host=x86_64-w64-mingw32
        MINGW_TOOL_PREFIX=x86_64-w64-mingw32-
        ;;
    *)
        build=i686-pc-cygwin
        host=i686-w64-mingw32
        MINGW_TOOL_PREFIX=i686-w64-mingw32-
        ;;
esac

godi_dir=/opt/wodi${WODI_ARCH}
export PATH=$godi_dir/sbin:$godi_dir/bin:$PATH

export AR=${MINGW_TOOL_PREFIX}ar.exe
export AS=${MINGW_TOOL_PREFIX}as.exe
export CC=${MINGW_TOOL_PREFIX}gcc.exe
export CPP=${MINGW_TOOL_PREFIX}cpp.exe
export CPPFILT=${MINGW_TOOL_PREFIX}c++filt.exe
export CXX=${MINGW_TOOL_PREFIX}g++.exe
export DLLTOOL=${MINGW_TOOL_PREFIX}dlltool.exe
export DLLWRAP=${MINGW_TOOL_PREFIX}dllwrap.exe
export GCOV=${MINGW_TOOL_PREFIX}gcov.exe
export LD=${MINGW_TOOL_PREFIX}ld.exe
export NM=${MINGW_TOOL_PREFIX}nm.exe
export OBJCOPY=${MINGW_TOOL_PREFIX}objcopy.exe
export OBJDUMP=${MINGW_TOOL_PREFIX}objdump.exe
export RANLIB=${MINGW_TOOL_PREFIX}ranlib.exe
export RC=${MINGW_TOOL_PREFIX}windres.exe
export READELF=${MINGW_TOOL_PREFIX}readelf.exe
export SIZE=${MINGW_TOOL_PREFIX}size.exe
export STRINGS=${MINGW_TOOL_PREFIX}strings.exe
export STRIP=${MINGW_TOOL_PREFIX}strip.exe
export WINDMC=${MINGW_TOOL_PREFIX}windmc.exe
export WINDRES=${MINGW_TOOL_PREFIX}windres.exe

# findlib is already installed


if [ $build_libffi -ne 0 ]; then
    # libffi:  we need a static version and only a static version
    (
        rm -rf /usr/local
        mkdir -p /usr/local/include
        wget ftp://sourceware.org/pub/libffi/libffi-${libffi_version}.tar.gz
        rm -rf libffi-${libffi_version}
        tar xfvz libffi-${libffi_version}.tar.gz
        cd libffi-${libffi_version}
        (./configure --build="$build" --host="$host" --prefix /usr/local --disable-shared --enable-static </dev/null && make </dev/null && make install </dev/null) || cat config.log
        mkdir -p /usr/local/include/
        ln -s -t /usr/local/include/ /usr/local/lib/libffi-${libffi_version}/include/*
    )

    export LIBFFI_CFLAGS="-I/usr/local/include"
    export LIBFFI_LIBS="-L/usr/local/lib -lffi"

else
    export LIBFFI_CFLAGS="-I${godi_dir}/include"
    export LIBFFI_LIBS="-L${godi_dir}/lib -lffi"
    export PKG_CONFIG_LIBDIR=${godi_dir}/lib/pkgconfig
fi

touch libffi.config
make distclean || true
rm -f libffi.config
make all
make date date-stubs date-stub-generator date-cmd-build date-cmd
./_build/date-cmd.native
./_build/date.native
if ! (make -k test 2>&1 | tee test.log; test ${PIPESTATUS[0]} -eq 0) ; then
    echo "test case failure" >&2
    exit 1
fi
