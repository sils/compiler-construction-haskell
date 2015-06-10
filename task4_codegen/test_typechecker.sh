# Error out when any command returns nonzero
set -e

make

GOODFILES=lab2-testsuite/good/*
for f in $GOODFILES
do
    ./TestCPP $f
done

BADFILES=lab2-testsuite/bad/*
for f in $BADFILES
do
    if ./TestCPP $f; then
        echo "File $f was successfully type checked but it is a bad example!"
        exit 1
    fi
done
