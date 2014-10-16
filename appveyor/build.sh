#!/usr/bin/env bash
set -ex

type -p ocamlc
ocamlc -version

x="$(uname -m)"
case "$x" in
    x86_64)
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
export STRINGS=${MINGW_TOOL_PREFIX}strip.exe
export STRIP=${MINGW_TOOL_PREFIX}strip.exe
export WINDMC=${MINGW_TOOL_PREFIX}windmc.exe
export WINDRES=${MINGW_TOOL_PREFIX}windres.exe

# findlib is already installed

# libffi:  we need a static version and only a static version
(
  rm -rf /usr/local
  mkdir -p /usr/local/include
  wget ftp://sourceware.org/pub/libffi/libffi-3.1.tar.gz
  rm -rf libffi-3.1
  tar xfvz libffi-3.1.tar.gz
  cd libffi-3.1
  (./configure --build="$build" --host="$host" --prefix /usr/local --disable-shared --enable-static </dev/null && make </dev/null && make install </dev/null) || cat config.log
  mkdir -p /usr/local/include/
  ln -s -t /usr/local/include/ /usr/local/lib/libffi-3.1/include/*
)

export LIBFFI_CFLAGS="-I/usr/local/include"
export LIBFFI_LIBS="-L/usr/local/lib -lffi"

touch setup.data
make distclean || true
rm -f setup.data
make all
if ! make -k test &>test.log ; then
    echo "test case failure" >&2
    exit 1
fi
