#!/bin/sh

# Error out when any command returns nonzero
set -e

rm -rf build
mkdir build
cd build
~/.cabal/bin/bnfc -m ../CPP.cf
make all
cat ../foo.cc|./TestCPP

# Task 1.2
cd ../
mtlPkg=`ghc-pkg latest mtl`
echo "Found mtl package $mtlPkg"
if [ $mtlPkg \< "mtl-2.2" ]
  then
    echo "running test in llvm-hs"
    cd llvm-hs
  else
    echo "running test in llvm-hs_mtl2.2"
    cd llvm-hs_mtl2.2
fi

make clean
make test
