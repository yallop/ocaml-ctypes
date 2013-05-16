#!/bin/sh -e
ocamlopt unix.cmxa clib.c cstub.c direct.ml -o direct
ocamlopt -shared -o cstub.so cstub.c
ocamlfind ocamlopt -verbose -linkpkg -package unix -package ctypes clib.c cstub.c indirect.ml -o indirect
./direct
./indirect
rm -f direct indirect *.so *.cmx *.cmi *.o
