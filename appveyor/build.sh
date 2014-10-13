#!/usr/bin/env bash
set -ex

type -p ocamlc
ocamlc -version

# libffi
(
  wget ftp://sourceware.org/pub/libffi/libffi-3.1.tar.gz
  tar xfvz libffi-3.1.tar.gz
  cd libffi-3.1
  (./configure  </dev/null && make </dev/null && make install </dev/null) || cat config.log
  mkdir -p /usr/local/include/
  ln -s -t /usr/local/include/ /usr/local/lib/libffi-3.1/include/*
)

# findlib
(
  wget http://download.camlcity.org/download/findlib-1.5.3.tar.gz
  tar xvfz findlib-1.5.3.tar.gz
  patch -p1 < appveyor/findlib-patch-create-process.patch 
  cd findlib-1.5.3
  ./configure && make all opt install
)

# oUnit
(
  wget -O ounit-2.0.0.tar.gz http://ftp.de.debian.org/debian/pool/main/o/ounit/ounit_2.0.0.orig.tar.gz
  tar xvfz ounit-2.0.0.tar.gz
  cd ounit-2.0.0
  ./configure && make && make install
)

make all
make -k test &>test.log || true
