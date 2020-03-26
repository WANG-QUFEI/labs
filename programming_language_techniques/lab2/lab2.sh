#!/bin/sh

# A simple shell program to run PLT CMM on Linux/Unix systems.

# Adds the current dir to the class path so that CMM.class
# is found even when called from another directory.

# Rename this to  CMM  to use with the testsuite.

dir=`dirname $0`
exec java -cp "$dir:$CLASSPATH" CMM "$@"
