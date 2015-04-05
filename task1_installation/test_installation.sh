#!/bin/sh

# Error out when any command returns nonzero
set -e

rm -rf build
mkdir build
cd build
~/.cabal/bin/bnfc -m ../CPP.cf
make all
cat ../foo.cc|./TestCPP
