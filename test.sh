#!/bin/bash

ghc --make -o eval main.hs || exit


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
'Parse error at "lisp" (line 1, column 32):
unexpected end of input
expecting space or ")"' \
    || exit


# Error for undefined function
./eval '(undefinedfunction 42)' | should_be 'Unrecognized primitive function: "undefinedfunction"' || exit


# Arithmetic primitive functions
./eval '(+ 5 2)' | should_be '7' || exit
./eval '(- 5 2)' | should_be '3' || exit
./eval '(/ 5 2)' | should_be '2' || exit
./eval '(* 5 2)' | should_be '10' || exit
./eval '(mod 5 2)' | should_be '1' || exit
./eval '(quotient 5 2)' | should_be '2' || exit
./eval '(remainder 5 2)' | should_be '1' || exit


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

./eval '(symbol?)' | should_be 'Expected 1 args; found values ()' || exit
./eval '(symbol? 42 42)' | should_be 'Expected 1 args; found values (42 42)' || exit


# symbol handling functions
./eval '(symbol->string abc)' | should_be '"abc"' || exit
./eval '(symbol->string 42)' | should_be 'Invalid type: expected Atom, found 42' || exit
./eval '(symbol->string)' | should_be 'Expected 1 args; found values ()' || exit
./eval '(symbol->string abc 42)' | should_be 'Expected 1 args; found values (abc 42)' || exit

./eval '(string->symbol "abc")' | should_be 'abc' || exit
./eval '(string->symbol 42)' | should_be 'Invalid type: expected String, found 42' || exit
./eval '(string->symbol)' | should_be 'Expected 1 args; found values ()' || exit
./eval '(string->symbol abc 42)' | should_be 'Expected 1 args; found values (abc 42)' || exit


# Comparison operators
./eval '(= 42 abc)' | should_be 'Invalid type: expected number, found abc' || exit
./eval '(= 2 abc 123)' | should_be 'Expected 2 args; found values (2 abc 123)' || exit

./eval '(= 42 42)' | should_be '#t' || exit
./eval '(= 42 0)' | should_be '#f' || exit

./eval '(< 2 3)' | should_be '#t' || exit
./eval '(< 3 2)' | should_be '#f' || exit

./eval '(> 3 2)' | should_be '#t' || exit
./eval '(> 2 3)' | should_be '#f' || exit

./eval '(/= 42 42)' | should_be '#f' || exit
./eval '(/= 42 0)' | should_be '#t' || exit

./eval '(<= 2 3)' | should_be '#t' || exit
./eval '(<= 2 2)' | should_be '#t' || exit
./eval '(<= 3 2)' | should_be '#f' || exit

./eval '(>= 3 2)' | should_be '#t' || exit
./eval '(>= 3 3)' | should_be '#t' || exit
./eval '(>= 2 3)' | should_be '#f' || exit

./eval '(&& #t #t)' | should_be '#t' || exit
./eval '(&& #t #f)' | should_be '#f' || exit
./eval '(&& #f #t)' | should_be '#f' || exit
./eval '(&& #f #f)' | should_be '#f' || exit

./eval '(|| #t #t)' | should_be '#t' || exit
./eval '(|| #t #f)' | should_be '#t' || exit
./eval '(|| #f #t)' | should_be '#t' || exit
./eval '(|| #f #f)' | should_be '#f' || exit

./eval '(string=? "hello" "hello")' | should_be '#t' || exit
./eval '(string=? "hello" "abc")' | should_be '#f' || exit

./eval '(string<? "h" "hello")' | should_be '#t' || exit
./eval '(string<? "hello" "h")' | should_be '#f' || exit
./eval '(string<? "hello" "hello")' | should_be '#f' || exit

./eval '(string>? "hello" "h")' | should_be '#t' || exit
./eval '(string>? "h" "hello")' | should_be '#f' || exit
./eval '(string>? "hello" "hello")' | should_be '#f' || exit

./eval '(string<=? "h" "hello")' | should_be '#t' || exit
./eval '(string<=? "hello" "h")' | should_be '#f' || exit
./eval '(string<=? "hello" "hello")' | should_be '#t' || exit

./eval '(string>=? "hello" "h")' | should_be '#t' || exit
./eval '(string>=? "h" "hello")' | should_be '#f' || exit
./eval '(string>=? "hello" "hello")' | should_be '#t' || exit



# If we haven't exited yet then all tests must have passed
echo "All tests passed"
