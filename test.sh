#!/bin/bash

function should_be() {
    ACTUAL=`cat /dev/stdin`
    #read -r ACTUAL  # read stdin of this function (from pipe)
    EXPECTED=$1
    if [ "$ACTUAL" != "$EXPECTED" ]
    then
        echo "Got '$ACTUAL', expected '$EXPECTED'"
        exit 1
    fi
}

function ignore_first_line() {
    tail -n +2
}


function parser_test {
    ./parser "$1" | ignore_first_line | should_be "$2" || exit
}



# Numbers
parser_test '25' '25' || exit
parser_test '#x10' '16' || exit
parser_test '#o10' '8' || exit
parser_test '#d10' '10' || exit
parser_test '#b10' '2' || exit

# Strings
parser_test '"hello world"' '"hello world"' || exit
parser_test '"hello \"world\""' '"hello "world""' || exit

# Atoms
parser_test 'abc' 'abc' || exit

# Characters
parser_test '#\a' '#\a' || exit
parser_test '(#\space)' "(#\ )" || exit

# Lists
parser_test "(a test)" \
    'List [Atom "a",Atom "test"]' \
    || exit
parser_test "(a (nested) test)" \
    'List [Atom "a",List [Atom "nested"],Atom "test"]' \
    || exit
parser_test "(a (dotted . list) test)" \
    'List [Atom "a",DottedList [Atom "dotted"] (Atom "list"),Atom "test"]' \
    || exit
parser_test "(a '(quoted (dotted . list)) test)" \
    'List [Atom "a",List [Atom "quote",List [Atom "quoted",DottedList [Atom "dotted"] (Atom "list")]],Atom "test"]' \
    || exit
parser_test "(a '(imbalanced parens)" \
    'unexpected end of input' \
    || exit


# If we haven't exited yet then all tests must have passed
echo "All tests passed"
