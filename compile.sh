#!/bin/bash

if [ -z $1 ]
then
    EXECUTABLE_NAME="lisp"
else
    EXECUTABLE_NAME=$1
fi

ghc --make -o bin/$EXECUTABLE_NAME  -outputdir tmp  src/Main.hs
