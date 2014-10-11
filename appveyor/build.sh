#!/usr/bin/env bash
set -ex

type -p ocamlc
ocamlc -version
cygcheck -l libffi6

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
  cd findlib-1.5.3
  ./configure && make all opt install
)

make all
