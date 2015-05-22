# Error out when any command returns nonzero
set -e

make

FILES=lab2-testsuite/good/*
for f in $FILES
do
    ./TestCPP $f
done
