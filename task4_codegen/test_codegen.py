#!/bin/python3

from subprocess import call as _call

def call(command):
    return _call(command, shell=True)

def main():
    call("make")
    call("./TestCPP ./testSuite/codeGenTest.c")
    for i in range(8):
        args = i*" anyarg"
        assert call("lli TestOutput.ll"+args) == i+2

if __name__ == "__main__":
    main()
