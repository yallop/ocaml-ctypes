#!/usr/bin/env bash
set -ex

WODI_ARCH=$1
MINGW_ARCH=$2
WODI_FILE=$3

echo "WODI_ARCH: ${WODI_ARCH}"
echo "MINGW_ARCH: ${MINGW_ARCH}"
echo "WODI_FILE: ${WODI_FILE}"

cp C:/${WODI_FILE} /tmp

pushd /tmp
rm -rf wodi${WODI_ARCH}
tar -xf wodi${WODI_ARCH}.tar.xz

wodi${WODI_ARCH}/install.sh

godi_dir=/opt/wodi${WODI_ARCH}
export PATH=$godi_dir/sbin:$godi_dir/bin:$PATH
godi_add godi-ounit base-libffi
popd


