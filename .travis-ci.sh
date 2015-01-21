OPAM_DEPENDS="ocamlfind ounit"
case "$OCAML_VERSION" in
4.00.1) ppa=avsm/ocaml40+opam12 ;;
4.01.0) ppa=avsm/ocaml41+opam12 ;;
4.02.1) ppa=avsm/ocaml42+opam12 ;;
*) echo Unknown $OCAML_VERSION; exit 1 ;;
esac

install_on_linux () {
  echo "yes" | sudo add-apt-repository ppa:$ppa
  sudo apt-get update -qq
  sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
  opam init
  opam update
}

install_on_osx () {
  curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
  sudo hdiutil attach XQuartz-2.7.6.dmg
  sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
  brew update
  brew install re2c libffi objective-caml camlp4 xz gringo scons bison
  brew upgrade boost cmake
  brew install --build-from-source opam
  opam init
  opam switch $OCAML_VERSION
  eval `opam config env` 
}

export OPAMYES=1
export OPAMVERBOSE=1

echo $TRAVIS_OS_NAME
case $TRAVIS_OS_NAME in
  osx) install_on_osx ;;
  linux) install_on_linux ;;
esac

echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version
opam install ${OPAM_DEPENDS}
eval `opam config env`
make
# build and run the tests
make test
# build and run the examples
make examples
_build/date.native
_build/date-cmd.native
_build/fts-cmd.native examples

# check Xen support builds too
set -eu
case "$OCAML_VERSION" in
4.00.*)
  echo "OCaml too old; not testing Mirage" ;;
*)
  if opam install mirage-xen; then
    make XEN=enable
    ls -l _build/libctypes_stubs_xen.a
  else
    echo "Mirage not installable, so not testing Xen build."
  fi ;;
esac
