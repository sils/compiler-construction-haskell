#!/bin/sh

# Error out when any command returns nonzero
set -e

echo Running tests for task 1...
cd task1_installation
sh test_installation.sh
cd ..
echo Done, tests for task 1.
echo Running tests for task 2..
cd task2_parser
sh test_task2.sh
cd ..
echo Done, tests for task 2.

