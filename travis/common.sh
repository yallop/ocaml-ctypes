export OPAMYES=1
export OPAMVERBOSE=1

echo $TRAVIS_OS_NAME
case $TRAVIS_OS_NAME in
  osx) ./travis/linux.sh
  linux) ./travis/osx.sh
esac
