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

function parser_test {
    ./eval "(quote $1)" | should_be "$2" || exit
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
    '(a test)' \
    || exit
parser_test "(a (nested) test)" \
    '(a (nested) test)' \
    || exit
parser_test "(a (dotted . list) test)" \
    '(a (dotted . list) test)' \
    || exit
parser_test "(a '(quoted (dotted . list)) test)" \
    '(a (quote (quoted (dotted . list))) test)' \
    || exit
parser_test "(a '(imbalanced parens)" \
'"No match: "lisp" (line 1, column 32):
unexpected end of input
expecting space or ")""' \
    || exit


# Type testing primitive functions
./eval '(symbol? abc)' | should_be '#t' || exit
./eval '(symbol? 42)' | should_be '#f' || exit

./eval '(string? "hello world")' | should_be '#t' || exit
./eval '(string? 42)' | should_be '#f' || exit

./eval '(number? 42)' | should_be '#t' || exit
./eval '(number? #x42)' | should_be '#t' || exit
./eval '(number? "hello world")' | should_be '#f' || exit

./eval '(char? #\a)' | should_be '#t' || exit
./eval '(char? #\newline)' | should_be '#t' || exit
./eval '(char? 42)' | should_be '#f' || exit

./eval '(bool? #t)' | should_be '#t' || exit
./eval '(bool? #f)' | should_be '#t' || exit
./eval '(bool? 42)' | should_be '#f' || exit

./eval "(list? '(1 2 3))" | should_be '#t' || exit
./eval '(list? 42)' | should_be '#f' || exit


# symbol handling functions
./eval '(symbol->string abc)' | should_be '"abc"' || exit
./eval '(string->symbol "abc")' | should_be 'abc' || exit


# If we haven't exited yet then all tests must have passed
echo "All tests passed"
