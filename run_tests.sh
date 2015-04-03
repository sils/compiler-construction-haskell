#!/bin/sh

# Error out when any command returns nonzero
set -e

echo Running tests for task 1...
cd task1_installation
sh test_installation.sh
echo Done.
