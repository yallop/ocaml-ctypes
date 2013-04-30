CFLAGS=-fPIC -g

all: build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure --enable-tests

build: setup.data setup.ml ctestlib
	ocaml setup.ml -build

install: setup.data setup.ml
	ocaml setup.ml -install

test: setup.ml build ctestlib
	ocaml setup.ml -test -verbose

distclean: setup.ml
	ocaml setup.ml -distclean
	rm -f tests/clib/test_functions.so tests/clib/test_functions.o

clean: setup.ml
	ocaml setup.ml -clean
	rm -f tests/clib/test_functions.so tests/clib/test_functions.o

ctestlib: tests/clib/test_functions.so

tests/clib/test_functions.so: tests/clib/test_functions.o
	cd tests/clib && cc -g -shared -o test_functions.so test_functions.o
