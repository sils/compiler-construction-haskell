#!/bin/python3

from subprocess import call as _call

def call(command):
    return _call(command, shell=True)

def main():
    try:
        assert call("make") == 0
        assert call("./TestCPP ./testSuite/codeGenTest.c") == 0
        for i in range(8):
            args = i*" anyarg"
            assert call("lli TestOutput.ll"+args) == i+2
    except AssertionError:
        print("Test failed!")
        exit(1)

    print("The code generator works according to this test suite.")

if __name__ == "__main__":
    main()
