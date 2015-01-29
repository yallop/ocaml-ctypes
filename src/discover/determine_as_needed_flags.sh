#!/bin/sh

touch as_needed_test.ml
if ocamlopt -shared -cclib -Wl,--no-as-needed as_needed_test.ml -o as_needed_test.cmxs 2>/dev/null
then
    echo 'as_needed_flags=-Wl,--no-as-needed'
else
    echo 'as_needed_flags='
fi
rm as_needed_test.*
