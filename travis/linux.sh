case "$OCAML_VERSION" in
4.01.0) ppa=avsm/ocaml41+opam12 ;;
4.02.3) ppa=avsm/ocaml42+opam12 ;;
*) ppa=avsm/ocaml42+opam12; use_opam=true ;;
esac

# install
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
if test $use_opam; then
    sudo apt-get install -qq opam
    opam init
    opam update
    opam switch -q $OCAML_VERSION
else
    sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
    opam init
    opam update
fi
eval `opam config env`

# Optional dependencies for coverage testing
if test $COVERAGE ; then
    opam install bisect_ppx ocveralls
fi

# Optional dependencies for Xen build
opam install mirage-xen || echo "Mirage not installable, so not testing Xen build."

opam pin add -n ctypes $(pwd)
opam install --build-test --yes ctypes

# Check that the inverted stubs package builds with this release
opam pin add -n ctypes-inverted-stubs-example https://github.com/yallop/ocaml-ctypes-inverted-stubs-example.git 
if test ! $COVERAGE && opam install --show-actions ctypes-inverted-stubs-example; then
    opam install --build-test --yes ctypes-inverted-stubs-example
else
    echo "Pinning the inverted stubs example failed, probably due to OCaml version incompatibility"
fi
