README
======

This folder contains task 4 for a compiler construction course at some weird
university.

This file can be viewed nicely in any viewer able to show markdown.

Whats in there
--------------

This fileset covers:

 * A grammar definition (given)
 * A typechecker, capable of typechecking programs written using that grammar
   (from last task)
 * A code generator (see CodeGenerator.hs)
 * Two execution scripts for the testsuite.

Running
-------

Just execute `./test_typechecker.sh` to get the typechecker from the last task
compiled and executed.

Execute `python3 test_codegen.py` to get the code generation tested. Look into
that file for further information about the code generator. The test code lies
under `testSuite/codeGenTest.c` and we hope we covered the most important cases.

Known Bugs
----------

`int a = -1` is not possible as the minus prefix is not supported by the
grammar. As the grammar was not given this was not fixed. You will see `0-1` in
such cases in the test file instead.

Authors
-------

This task was done by Timo Neuhaeusser, Jonas Ackermann and Lasse Schuirmann.
