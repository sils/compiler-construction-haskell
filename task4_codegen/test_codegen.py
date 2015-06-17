#!/bin/python3

from subprocess import call

def main():
    call("make", shell=True)
    call("./TestCPP ./testSuite/codeGenTest.c", shell=True)
    for i in range(8):
        args = i*" anyarg"
        assert call("lli TestOutput.ll"+args, shell=True) == i+2

if __name__ == "__main__":
    main()
