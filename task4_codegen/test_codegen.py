#!/bin/python3

from subprocess import call as _call

def call(command):
    return _call(command, shell=True)

def main():
    try:
        print("Building compiler...", end=" ")
        assert call("make > /dev/null") == 0
        print("DONE!")
        print("Compiling testcode...", end=" ")
        assert call("./TestCPP ./testSuite/codeGenTest.c  > /dev/null") == 0
        print("DONE!")
        for i in range(8):
            command = "lli TestOutput.ll"+i*" anyarg"
            print("Calling '{}'...".format(command), end=" ")
            assert call(command) == i+2
            print("DONE!")
    except AssertionError:
        print("Test failed!")
        exit(1)

    print("The code generator works according to this test suite.")

if __name__ == "__main__":
    main()
