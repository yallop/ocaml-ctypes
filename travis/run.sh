#!/bin/bash -ex

export OPAMYES=1

eval $(opam config env)

gringo --version
aspcud --version
opam --version
type ocamlc


cd /ctypes
opam pin add -n ctypes $(pwd)

opam show --raw ctypes

opam install --build-test --verbose ctypes


# Check that the inverted stubs package builds with this revision
opam pin add -n ctypes-inverted-stubs-example https://github.com/yallop/ocaml-ctypes-inverted-stubs-example.git 
if opam install --show-actions ctypes-inverted-stubs-example; then
    opam install --build-test ctypes-inverted-stubs-example
else
    echo "Pinning the inverted stubs example failed, probably due to OCaml version incompatibility"
fi
