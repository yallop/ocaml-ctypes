#!/bin/sh -e
ocamlopt unix.cmxa clib.c cstub.c time.ml int_direct.ml -o direct
ocamlfind ocamlopt -verbose -linkpkg -package unix -package ctypes clib.c cstub.c time.ml int_indirect.ml -o indirect
./direct
./indirect
rm -f direct indirect *.so *.cmx *.cmi *.o
