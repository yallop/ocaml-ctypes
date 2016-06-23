#!/bin/bash

echo $TRAVIS_OS_NAME
case $TRAVIS_OS_NAME in
    osx) ;;
    linux) sudo apt-get update && sudo apt-get install --yes qemu-user-static
           docker pull yallop/ocaml-ctypes-$DOCKERFILE
           ;;
esac

