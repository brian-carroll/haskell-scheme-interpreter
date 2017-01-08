#!/bin/bash

ghc --make -o eval main.hs

trap "exit 1" TERM
export TOP_PID=$$

function should_be() {
    ACTUAL=`cat /dev/stdin`
    #read -r ACTUAL  # read stdin of this function (from pipe)
    EXPECTED=$1
    if [ "$ACTUAL" != "$EXPECTED" ]
    then
        echo "Got '$ACTUAL', expected '$EXPECTED'"
        kill -s TERM $TOP_PID
    fi
}

function parser_test {
    ./eval "(quote $1)" | should_be "$2"
}



# Numbers
parser_test '25' '25'
parser_test '#x10' '16'
parser_test '#o10' '8'
parser_test '#d10' '10'
parser_test '#b10' '2'

# Strings
parser_test '"hello world"' '"hello world"'
parser_test '"hello \"world\""' '"hello "world""'

# Atoms
parser_test 'abc' 'abc'

# Characters
parser_test '#\a' '#\a'
parser_test '(#\space)' "(#\ )"

# Lists
parser_test "(a test)" \
    '(a test)'

parser_test "(a (nested) test)" \
    '(a (nested) test)'

parser_test "(a (dotted . list) test)" \
    '(a (dotted . list) test)'

parser_test "(a '(quoted (dotted . list)) test)" \
    '(a (quote (quoted (dotted . list))) test)'

parser_test "(a '(imbalanced parens)" \
'Parse error at "lisp" (line 1, column 32):
unexpected end of input
expecting space or ")"'



# Error for undefined function
./eval '(undefinedfunction 42)' | should_be 'Unrecognized primitive function: "undefinedfunction"'


# Arithmetic primitive functions
./eval '(+ 5 2)' | should_be '7'
./eval '(- 5 2)' | should_be '3'
./eval '(/ 5 2)' | should_be '2'
./eval '(* 5 2)' | should_be '10'
./eval '(mod 5 2)' | should_be '1'
./eval '(quotient 5 2)' | should_be '2'
./eval '(remainder 5 2)' | should_be '1'


# Type testing primitive functions
./eval '(symbol? abc)' | should_be '#t'
./eval '(symbol? 42)' | should_be '#f'

./eval '(string? "hello world")' | should_be '#t'
./eval '(string? 42)' | should_be '#f'

./eval '(number? 42)' | should_be '#t'
./eval '(number? #x42)' | should_be '#t'
./eval '(number? "hello world")' | should_be '#f'

./eval '(char? #\a)' | should_be '#t'
./eval '(char? #\newline)' | should_be '#t'
./eval '(char? 42)' | should_be '#f'

./eval '(bool? #t)' | should_be '#t'
./eval '(bool? #f)' | should_be '#t'
./eval '(bool? 42)' | should_be '#f'

./eval "(list? '(1 2 3))" | should_be '#t'
./eval '(list? 42)' | should_be '#f'

./eval '(symbol?)' | should_be 'Expected 1 args; found values ()'
./eval '(symbol? 42 42)' | should_be 'Expected 1 args; found values (42 42)'


# symbol handling functions
./eval '(symbol->string abc)' | should_be '"abc"'
./eval '(symbol->string 42)' | should_be 'Invalid type: expected Atom, found 42'
./eval '(symbol->string)' | should_be 'Expected 1 args; found values ()'
./eval '(symbol->string abc 42)' | should_be 'Expected 1 args; found values (abc 42)'

./eval '(string->symbol "abc")' | should_be 'abc'
./eval '(string->symbol 42)' | should_be 'Invalid type: expected String, found 42'
./eval '(string->symbol)' | should_be 'Expected 1 args; found values ()'
./eval '(string->symbol abc 42)' | should_be 'Expected 1 args; found values (abc 42)'


# Comparison operators
./eval '(= 42 abc)' | should_be 'Invalid type: expected number, found abc'
./eval '(= 2 abc 123)' | should_be 'Expected 2 args; found values (2 abc 123)'

./eval '(= 42 42)' | should_be '#t'
./eval '(= 42 0)' | should_be '#f'

./eval '(< 2 3)' | should_be '#t'
./eval '(< 3 2)' | should_be '#f'

./eval '(> 3 2)' | should_be '#t'
./eval '(> 2 3)' | should_be '#f'

./eval '(/= 42 42)' | should_be '#f'
./eval '(/= 42 0)' | should_be '#t'

./eval '(<= 2 3)' | should_be '#t'
./eval '(<= 2 2)' | should_be '#t'
./eval '(<= 3 2)' | should_be '#f'

./eval '(>= 3 2)' | should_be '#t'
./eval '(>= 3 3)' | should_be '#t'
./eval '(>= 2 3)' | should_be '#f'

./eval '(&& #t #t)' | should_be '#t'
./eval '(&& #t #f)' | should_be '#f'
./eval '(&& #f #t)' | should_be '#f'
./eval '(&& #f #f)' | should_be '#f'

./eval '(|| #t #t)' | should_be '#t'
./eval '(|| #t #f)' | should_be '#t'
./eval '(|| #f #t)' | should_be '#t'
./eval '(|| #f #f)' | should_be '#f'

./eval '(string=? "hello" "hello")' | should_be '#t'
./eval '(string=? "hello" "abc")' | should_be '#f'

./eval '(string<? "h" "hello")' | should_be '#t'
./eval '(string<? "hello" "h")' | should_be '#f'
./eval '(string<? "hello" "hello")' | should_be '#f'

./eval '(string>? "hello" "h")' | should_be '#t'
./eval '(string>? "h" "hello")' | should_be '#f'
./eval '(string>? "hello" "hello")' | should_be '#f'

./eval '(string<=? "h" "hello")' | should_be '#t'
./eval '(string<=? "hello" "h")' | should_be '#f'
./eval '(string<=? "hello" "hello")' | should_be '#t'

./eval '(string>=? "hello" "h")' | should_be '#t'
./eval '(string>=? "h" "hello")' | should_be '#f'
./eval '(string>=? "hello" "hello")' | should_be '#t'

./eval '(if (> 2 3) "yes" "no")' | should_be '"no"'
./eval '(if (< 2 3) "yes" "no")' | should_be '"yes"'
./eval '(if 1 2 3 4)' | should_be 'Expected 3 args; found values (1 2 3 4)'
./eval '(if 1 2)' | should_be 'Expected 3 args; found values (1 2)'

./eval "(car '(1 2 3))" | should_be '1'
./eval "(car '(1))" | should_be '1'
./eval "(car '(1 2 . 3))" | should_be '1'
./eval '(car "thing")' | should_be 'Invalid type: expected pair, found "thing"'
./eval '(car 1 2 3)' | should_be 'Expected 1 args; found values (1 2 3)'

./eval "(cdr '(a b c))" | should_be '(b c)'
./eval "(cdr '(a b))" | should_be '(b)'
./eval "(cdr '(a))" | should_be '()'
./eval "(cdr '(a . b))" | should_be 'b'
./eval "(cdr '(a b . c))" | should_be '(b . c)'
./eval "(cdr 'a)" | should_be 'Invalid type: expected pair, found a'
./eval "(cdr 'a 'b)" | should_be 'Expected 1 args; found values (a b)'

./eval "(cons 1 ())" | should_be '(1)'
./eval "(cons 1 '(2 3))" | should_be '(1 2 3)'
./eval "(cons 1 '(2 . 3))" | should_be '(1 2 . 3)'
./eval "(cons 1 2)" | should_be '(1 . 2)'
./eval "(cons '(1 2) 3)" | should_be '((1 2) . 3)'
./eval "(cons 1 2 3)" | should_be 'Expected 2 args; found values (1 2 3)'
./eval "(cons 1)" | should_be 'Expected 2 args; found values (1)'


# If we haven't exited yet then all tests must have passed
echo "All tests passed"
