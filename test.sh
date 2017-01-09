#!/bin/bash

./compile.sh lisp

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
    bin/lisp "(quote $1)" | should_be "$2"
}

function repl {
    local MULTIPLE_LISP_EXPRS=$1
    echo -e $MULTIPLE_LISP_EXPRS | bin/lisp 2>/dev/null
}

function remove_prompt {
    local INPUT=$(cat /dev/stdin)
    echo ${INPUT#"Lisp>>> "}
}

function last_result {
    # Extract second-last line (omit the blank line at the end)
    tail -n 2 | head -n 1 | remove_prompt
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
bin/lisp '(undefinedfunction 42)' | should_be 'Unrecognized primitive function: "undefinedfunction"'


# Arithmetic primitive functions
bin/lisp '(+ 5 2)' | should_be '7'
bin/lisp '(- 5 2)' | should_be '3'
bin/lisp '(/ 5 2)' | should_be '2'
bin/lisp '(* 5 2)' | should_be '10'
bin/lisp '(mod 5 2)' | should_be '1'
bin/lisp '(quotient 5 2)' | should_be '2'
bin/lisp '(remainder 5 2)' | should_be '1'


# Type testing primitive functions
bin/lisp "(symbol? 'abc)" | should_be '#t'
bin/lisp '(symbol? 42)' | should_be '#f'

bin/lisp '(string? "hello world")' | should_be '#t'
bin/lisp '(string? 42)' | should_be '#f'

bin/lisp '(number? 42)' | should_be '#t'
bin/lisp '(number? #x42)' | should_be '#t'
bin/lisp '(number? "hello world")' | should_be '#f'

bin/lisp '(char? #\a)' | should_be '#t'
bin/lisp '(char? #\newline)' | should_be '#t'
bin/lisp '(char? 42)' | should_be '#f'

bin/lisp '(bool? #t)' | should_be '#t'
bin/lisp '(bool? #f)' | should_be '#t'
bin/lisp '(bool? 42)' | should_be '#f'

bin/lisp "(list? '(1 2 3))" | should_be '#t'
bin/lisp '(list? 42)' | should_be '#f'

bin/lisp '(symbol?)' | should_be 'Expected 1 args; found values ()'
bin/lisp '(symbol? 42 42)' | should_be 'Expected 1 args; found values (42 42)'


# symbol handling functions
bin/lisp "(symbol->string 'abc)" | should_be '"abc"'
bin/lisp '(symbol->string 42)' | should_be 'Invalid type: expected Atom, found 42'
bin/lisp '(symbol->string)' | should_be 'Expected 1 args; found values ()'
bin/lisp "(symbol->string 'abc 42)" | should_be 'Expected 1 args; found values (abc 42)'

bin/lisp '(string->symbol "abc")' | should_be 'abc'
bin/lisp '(string->symbol 42)' | should_be 'Invalid type: expected String, found 42'
bin/lisp '(string->symbol)' | should_be 'Expected 1 args; found values ()'
bin/lisp '(string->symbol "abc" 42)' | should_be 'Expected 1 args; found values ("abc" 42)'


# Comparison operators
bin/lisp '(= 42 "abc")' | should_be 'Invalid type: expected number, found "abc"'
bin/lisp '(= 2 "abc" 123)' | should_be 'Expected 2 args; found values (2 "abc" 123)'

bin/lisp '(= 42 42)' | should_be '#t'
bin/lisp '(= 42 0)' | should_be '#f'

bin/lisp '(< 2 3)' | should_be '#t'
bin/lisp '(< 3 2)' | should_be '#f'

bin/lisp '(> 3 2)' | should_be '#t'
bin/lisp '(> 2 3)' | should_be '#f'

bin/lisp '(/= 42 42)' | should_be '#f'
bin/lisp '(/= 42 0)' | should_be '#t'

bin/lisp '(<= 2 3)' | should_be '#t'
bin/lisp '(<= 2 2)' | should_be '#t'
bin/lisp '(<= 3 2)' | should_be '#f'

bin/lisp '(>= 3 2)' | should_be '#t'
bin/lisp '(>= 3 3)' | should_be '#t'
bin/lisp '(>= 2 3)' | should_be '#f'

bin/lisp '(&& #t #t)' | should_be '#t'
bin/lisp '(&& #t #f)' | should_be '#f'
bin/lisp '(&& #f #t)' | should_be '#f'
bin/lisp '(&& #f #f)' | should_be '#f'

bin/lisp '(|| #t #t)' | should_be '#t'
bin/lisp '(|| #t #f)' | should_be '#t'
bin/lisp '(|| #f #t)' | should_be '#t'
bin/lisp '(|| #f #f)' | should_be '#f'

bin/lisp '(string=? "hello" "hello")' | should_be '#t'
bin/lisp '(string=? "hello" "abc")' | should_be '#f'

bin/lisp '(string<? "h" "hello")' | should_be '#t'
bin/lisp '(string<? "hello" "h")' | should_be '#f'
bin/lisp '(string<? "hello" "hello")' | should_be '#f'

bin/lisp '(string>? "hello" "h")' | should_be '#t'
bin/lisp '(string>? "h" "hello")' | should_be '#f'
bin/lisp '(string>? "hello" "hello")' | should_be '#f'

bin/lisp '(string<=? "h" "hello")' | should_be '#t'
bin/lisp '(string<=? "hello" "h")' | should_be '#f'
bin/lisp '(string<=? "hello" "hello")' | should_be '#t'

bin/lisp '(string>=? "hello" "h")' | should_be '#t'
bin/lisp '(string>=? "h" "hello")' | should_be '#f'
bin/lisp '(string>=? "hello" "hello")' | should_be '#t'

bin/lisp '(if (> 2 3) "yes" "no")' | should_be '"no"'
bin/lisp '(if (< 2 3) "yes" "no")' | should_be '"yes"'
bin/lisp '(if 1 2 3 4)' | should_be 'Expected 3 args; found values (1 2 3 4)'
bin/lisp '(if 1 2)' | should_be 'Expected 3 args; found values (1 2)'

bin/lisp "(car '(1 2 3))" | should_be '1'
bin/lisp "(car '(1))" | should_be '1'
bin/lisp "(car '(1 2 . 3))" | should_be '1'
bin/lisp '(car "thing")' | should_be 'Invalid type: expected pair, found "thing"'
bin/lisp '(car 1 2 3)' | should_be 'Expected 1 args; found values (1 2 3)'

bin/lisp "(cdr '(a b c))" | should_be '(b c)'
bin/lisp "(cdr '(a b))" | should_be '(b)'
bin/lisp "(cdr '(a))" | should_be '()'
bin/lisp "(cdr '(a . b))" | should_be 'b'
bin/lisp "(cdr '(a b . c))" | should_be '(b . c)'
bin/lisp "(cdr 'a)" | should_be 'Invalid type: expected pair, found a'
bin/lisp "(cdr 'a 'b)" | should_be 'Expected 1 args; found values (a b)'

bin/lisp "(cons 1 ())" | should_be '(1)'
bin/lisp "(cons 1 '(2 3))" | should_be '(1 2 3)'
bin/lisp "(cons 1 '(2 . 3))" | should_be '(1 2 . 3)'
bin/lisp "(cons 1 2)" | should_be '(1 . 2)'
bin/lisp "(cons '(1 2) 3)" | should_be '((1 2) . 3)'
bin/lisp "(cons 1 2 3)" | should_be 'Expected 2 args; found values (1 2 3)'
bin/lisp "(cons 1)" | should_be 'Expected 2 args; found values (1)'

bin/lisp "(eqv? '(1 2 3) '(1 2 3))" | should_be '#t'
bin/lisp '(eqv? 2 2)' | should_be '#t'
bin/lisp '(eqv? 2 "2")' | should_be '#f'

bin/lisp "(equal? '(1 2 3) '(1 2 3))" | should_be '#t'
bin/lisp '(equal? 2 2)' | should_be '#t'
bin/lisp '(equal? 2 "2")' | should_be '#t'
bin/lisp '(equal? (quote 2) "2")' | should_be '#t'
bin/lisp '(equal? (quote (1 "2")) (quote (1 2)))' | should_be '#t' # recursive weak typing
bin/lisp '(equal? 2 3)' | should_be '#f'


# Variables (get, define, set)
repl '(+ x 1)' | last_result | should_be "Getting an unbound variable: x"
repl '(set! x 123)' | last_result | should_be "Setting an unbound variable: x"
repl '(define x 123)\n(+ x 1)' | last_result | should_be "124"
repl '(define x 123)\n(set! x 321)\n(+ x 1)' | last_result | should_be "322"
repl '(define x 123)\n(define x 321)\n(+ x 1)' | last_result | should_be "322"


# If we haven't exited yet then all tests must have passed
echo "All tests passed"
