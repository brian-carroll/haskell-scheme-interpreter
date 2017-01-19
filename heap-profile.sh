#!/bin/bash
./compile.sh -rtsopts  -prof -auto-all -caf-all 
bin/lisp tailrec.scm +RTS -p -hc -K100M
hp2ps -e8in -c lisp.hp 
