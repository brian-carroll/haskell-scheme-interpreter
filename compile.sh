#!/bin/bash

if [ "$1" == "--clean" ]
then
    shift
    find .ghc ! -name .gitignore -type f | xargs rm -f
fi

cd src
ghc --make \
    -o ../bin/lisp \
    -outputdir ../.ghc \
    -W \
    Main.hs \
    "$@"
