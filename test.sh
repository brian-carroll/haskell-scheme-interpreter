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


function simple_parser_test {
    ./simple_parser "$1" | ignore_first_line | should_be "$2" || exit
}



# Numbers
simple_parser_test '25' '25' || exit
simple_parser_test '#x10' '16' || exit
simple_parser_test '#o10' '8' || exit
simple_parser_test '#d10' '10' || exit
simple_parser_test '#b10' '2' || exit

# Strings
simple_parser_test '"hello world"' '"hello world"' || exit
simple_parser_test '"hello \"world\""' '"hello "world""' || exit

# Atoms
simple_parser_test 'abc' 'abc' || exit

# Characters
simple_parser_test '#\a' '#\a' || exit
simple_parser_test '(#\space)' "(#\ )" || exit

# Lists
simple_parser_test "(a test)" \
    'List [Atom "a",Atom "test"]' \
    || exit
simple_parser_test "(a (nested) test)" \
    'List [Atom "a",List [Atom "nested"],Atom "test"]' \
    || exit
simple_parser_test "(a (dotted . list) test)" \
    'List [Atom "a",DottedList [Atom "dotted"] (Atom "list"),Atom "test"]' \
    || exit
simple_parser_test "(a '(quoted (dotted . list)) test)" \
    'List [Atom "a",List [Atom "quote",List [Atom "quoted",DottedList [Atom "dotted"] (Atom "list")]],Atom "test"]' \
    || exit
simple_parser_test "(a '(imbalanced parens)" \
    'unexpected end of input' \
    || exit


# If we haven't exited yet then all tests must have passed
echo "All tests passed"