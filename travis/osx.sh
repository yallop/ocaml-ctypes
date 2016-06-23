
# install
curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
sudo hdiutil attach XQuartz-2.7.6.dmg
sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
brew update
brew reinstall ocaml
brew install libffi opam
opam init
opam switch $OCAML_VERSION
eval `opam config env` 

opam pin add -n ctypes $(pwd)
opam install --build-test --yes ctypes

# Check that the inverted stubs package builds with this release
opam pin add -n ctypes-inverted-stubs-example https://github.com/yallop/ocaml-ctypes-inverted-stubs-example.git 
if opam install --show-actions ctypes-inverted-stubs-example; then
    opam install --build-test --yes ctypes-inverted-stubs-example
else
    echo "Pinning the inverted stubs example failed, probably due to OCaml version incompatibility"
fi

 
