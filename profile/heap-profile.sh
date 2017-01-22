#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR/..
./compile.sh -rtsopts  -prof -auto-all -caf-all 
cd $DIR
"../bin/lisp" "$DIR/tailrec.scm" +RTS -p -hc -K100M
hp2ps -e8in -c lisp.hp 
