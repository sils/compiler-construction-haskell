#! /bin/bash

cd tests
ghc --make -o progs-test-lab1 progs-test-lab1.hs
./progs-test-lab1 ../grammar.cf
