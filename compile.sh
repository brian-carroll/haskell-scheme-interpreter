#!/bin/bash

if [ -z $1 ]
then
    EXECUTABLE_NAME="lisp"
else
    EXECUTABLE_NAME=$1
fi

cd src
ghc --make \
    -o ../bin/$EXECUTABLE_NAME \
    -outputdir ../tmp \
    Main.hs
