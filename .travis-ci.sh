if test $COVERAGE; then
    MAKE='make COVERAGE=true'
    if test "$TRAVIS_OS_NAME" != osx ; then
	USE_BISECT=true;
    fi
else
    MAKE='make'
fi

# build and run the examples
$MAKE examples
_build/date.native
_build/date-cmd.native
_build/fts-cmd.native examples

if test $USE_BISECT ; then
    ocveralls --send bisect*.out _build/bisect*.out > coveralls.json
fi

# check Xen support builds too
set -eu
if opam install mirage-xen; then
  make XEN=enable
  ls -l _build/libctypes_stubs_xen.a
else
  echo "Mirage not installable, so not testing Xen build."
fi
