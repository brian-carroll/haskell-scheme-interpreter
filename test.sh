#!/bin/bash

function should_be() {
    read -r ACTUAL  # read stdin of this function (from pipe)
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


simple_parser_test '25' 'Number 25' || exit

simple_parser_test '#x10' 'Number 16' || exit
simple_parser_test '#o10' 'Number 8' || exit
simple_parser_test '#d10' 'Number 10' || exit
simple_parser_test '#b10' 'Number 2' || exit

simple_parser_test '"hello world"' 'hello world' || exit
simple_parser_test '"hello \"world\""' 'hello "world"' || exit

simple_parser_test 'abc' 'Atom "abc"' || exit

simple_parser_test '#\newline' "Character '\n'" || exit
simple_parser_test '#\a' "Character 'a'" || exit



echo "All tests passed"
