ANDROID_REPOSITORY=git://github.com/whitequark/opam-cross-android
export OCAMLFINDFLAGS

case "$OCAML_VERSION" in
4.01.0) ppa=avsm/ocaml41+opam12 ;;
4.02.3) ppa=avsm/ocaml42+opam12 ;;
*) ppa=avsm/ocaml42+opam12; use_opam=true ;;
esac

install_on_linux () {
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
}

install_on_osx () {
  curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
  sudo hdiutil attach XQuartz-2.7.6.dmg
  sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
  brew update
  brew reinstall ocaml
  brew install libffi opam
  opam init
  opam switch install ocaml-base-compiler.$OCAML_VERSION
  eval `opam config env` 
}

install_android_toolchain () {
  sudo apt-get update -qq
  sudo apt-get install -qq gcc-multilib
  install_on_linux
  opam remote add android $ANDROID_REPOSITORY
  ARCH=arm SUBARCH=armv7 SYSTEM=linux_eabi \
    CCARCH=arm TOOLCHAIN=arm-linux-androideabi-4.9 \
    TRIPLE=arm-linux-androideabi LEVEL=24 \
    STLVER=4.9 STLARCH=armeabi \
    opam install conf-android
  opam install ocaml-android
  opam install integers-android
  OCAMLFINDFLAGS='-toolchain android'
}

export OPAMYES=1
export OPAMVERBOSE=1

echo $TRAVIS_OS_NAME
case $ANDROID in
  true) install_android_toolchain ;;
  *) case $TRAVIS_OS_NAME in
       osx) install_on_osx ;;
       linux) install_on_linux ;;
     esac
esac

echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

# Optional dependencies for coverage testing
if test $COVERAGE -a $TRAVIS_OS_NAME != osx ; then
    opam install bisect_ppx ocveralls
fi

# Optional dependencies for Xen build
opam install mirage-xen || echo "Mirage not installable, so not testing Xen build."

opam pin add -n ctypes $(pwd)
if test $ANDROID; then
	opam install --yes ctypes
else
	opam install --build-test --yes ctypes
fi

# Check that the inverted stubs package builds with this release
opam pin add -n ctypes-inverted-stubs-example https://github.com/yallop/ocaml-ctypes-inverted-stubs-example.git 
if test ! $ANDROID && test ! $COVERAGE && opam install --show-actions ctypes-inverted-stubs-example; then
    opam install --build-test --yes ctypes-inverted-stubs-example
else
    echo "Pinning the inverted stubs example failed, probably due to OCaml version incompatibility"
fi


