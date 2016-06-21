#!/bin/bash
cd $(dirname $0)
eval $(opam config env)
make
make test # build and run the tests
make examples # build and run the examples
_build/date.native
_build/date-cmd.native
_build/fts-cmd.native examples
