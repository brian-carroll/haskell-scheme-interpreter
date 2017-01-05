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


./simple_parser '25' | ignore_first_line | should_be 'Number 25' || exit

./simple_parser '#x10' | ignore_first_line | should_be 'Number 16' || exit
./simple_parser '#o10' | ignore_first_line | should_be 'Number 8' || exit
./simple_parser '#d10' | ignore_first_line | should_be 'Number 10' || exit
./simple_parser '#b10' | ignore_first_line | should_be 'Number 2' || exit

./simple_parser '"hello world"' | ignore_first_line | should_be 'hello world' || exit
./simple_parser '"hello \"world\""' | ignore_first_line | should_be 'hello "world"' || exit

./simple_parser 'abc' | ignore_first_line | should_be 'Atom "abc"' || exit

# ./simple_parser '#\newline' | ignore_first_line | should_be "Character '\n'" || exit
# ./simple_parser '#\a' | ignore_first_line | should_be "Character 'a'" || exit

echo "All tests passed"
