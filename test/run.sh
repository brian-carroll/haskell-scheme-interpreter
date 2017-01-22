#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
trap "exit 1" TERM
export TOP_PID=$$


"$DIR"/../compile.sh

function lisp {
    "$DIR/../bin/lisp" "$@"
}

function should_be() {
    ACTUAL=`cat /dev/stdin`
    EXPECTED=$1
    if [ "$ACTUAL" != "$EXPECTED" ]
    then
        echo "Got '$ACTUAL', expected '$EXPECTED'"
        kill -s TERM $TOP_PID
    fi
}

function parser_test {
    lisp "(quote $1)" | should_be "$2"
}

function remove_prompt {
    local INPUT=$(cat /dev/stdin)
    echo ${INPUT#"> "}
}

function last_result {
    # Extract second-last line (omit the blank line at the end)
    tail -n 2 | head -n 1 | remove_prompt
}


#____________________________________________________________________________

SCHEME_FILE="$DIR/closure.scm"
SCHEME_FILE_CONTENTS=$(cat $SCHEME_FILE)
INFILE=$SCHEME_FILE
OUTFILE="$DIR/output.txt"

# Command line: Load a Scheme file and execute it
lisp $SCHEME_FILE | should_be '8'

# File IO
lisp '(open-input-file "'$INFILE'")'                        | should_be '<IO port {handle: '$INFILE'} >'
lisp '(close-input-port (open-input-file "'$INFILE'"))'     | should_be '#t'

lisp '(open-output-file "'$OUTFILE'")'                      | should_be '<IO port {handle: '$OUTFILE'} >'
lisp '(close-output-port (open-output-file "'$OUTFILE'"))'  | should_be '#t'

lisp '(read (open-input-file "'$INFILE'"))'                 | should_be "$( head -n 1 $INFILE )"
lisp '(read-contents "'$SCHEME_FILE'")'                     | should_be "\"$SCHEME_FILE_CONTENTS
\""
lisp '(read-all "'$SCHEME_FILE'")' | should_be '((define (counter inc) (lambda (x) (set! inc (+ x inc)) inc)) (define my-count (counter 5)) (my-count 3))'

# Write to file
if ! rm $OUTFILE ; then
    echo "Failed to delete $OUTFILE" && exit
fi
echo '(define outfile (open-output-file "'$OUTFILE'"))
(write "hello world" outfile)
(close-output-port outfile)
quit' | lisp | last_result | should_be '#t'
cat $OUTFILE | should_be '"hello world"'

# Apply
echo '(define (f x y) (+ x y))
(apply f 1 2)
quit' | lisp | last_result | should_be '3'



# User-defined functions
echo "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(define my-count (counter 5))
(my-count 3)
quit" | lisp | last_result | should_be '8'

echo '(define (f x y) (+ x y))
(f 1 2 3)
quit' | lisp | last_result | should_be 'Expected 2 args; found values (1 2 3)'

echo '(define (f x y) (+ x y))
(f 1)
quit' | lisp | last_result | should_be 'Expected 2 args; found values (1)'



# Variables (get, define, set)
lisp '(+ x 1)' | last_result | should_be "Getting an unbound variable: x"
lisp '(set! x 123)' | last_result | should_be "Setting an unbound variable: x"

echo '(define x 123)
(+ x 1)
quit' | lisp | last_result | should_be "124"

echo '(define x 123)
(set! x 321)
(+ x 1)
quit' | lisp | last_result | should_be "322"

echo '(define x 123)
(define x 321)
(+ x 1)
quit' | lisp | last_result | should_be "322"


#____________________________________________________________________________


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
expecting space, ";" or ")"'


#____________________________________________________________________________


# Error for undefined function
lisp '(undefinedfunction 42)' | should_be 'Getting an unbound variable: undefinedfunction'


# Arithmetic primitive functions
lisp '(+ 5 2)' | should_be '7'
lisp '(- 5 2)' | should_be '3'
lisp '(/ 5 2)' | should_be '2'
lisp '(* 5 2)' | should_be '10'
lisp '(mod 5 2)' | should_be '1'
lisp '(quotient 5 2)' | should_be '2'
lisp '(remainder 5 2)' | should_be '1'


# Type testing primitive functions
lisp "(symbol? 'abc)" | should_be '#t'
lisp '(symbol? 42)' | should_be '#f'

lisp '(string? "hello world")' | should_be '#t'
lisp '(string? 42)' | should_be '#f'

lisp '(number? 42)' | should_be '#t'
lisp '(number? #x42)' | should_be '#t'
lisp '(number? "hello world")' | should_be '#f'

lisp '(char? #\a)' | should_be '#t'
lisp '(char? #\newline)' | should_be '#t'
lisp '(char? 42)' | should_be '#f'

lisp '(bool? #t)' | should_be '#t'
lisp '(bool? #f)' | should_be '#t'
lisp '(bool? 42)' | should_be '#f'

lisp "(list? '(1 2 3))" | should_be '#t'
lisp '(list? 42)' | should_be '#f'

lisp '(symbol?)' | should_be 'Expected 1 args; found values ()'
lisp '(symbol? 42 42)' | should_be 'Expected 1 args; found values (42 42)'


# symbol handling functions
lisp "(symbol->string 'abc)" | should_be '"abc"'
lisp '(symbol->string 42)' | should_be 'Invalid type: expected Atom, found 42'
lisp '(symbol->string)' | should_be 'Expected 1 args; found values ()'
lisp "(symbol->string 'abc 42)" | should_be 'Expected 1 args; found values (abc 42)'

lisp '(string->symbol "abc")' | should_be 'abc'
lisp '(string->symbol 42)' | should_be 'Invalid type: expected String, found 42'
lisp '(string->symbol)' | should_be 'Expected 1 args; found values ()'
lisp '(string->symbol "abc" 42)' | should_be 'Expected 1 args; found values ("abc" 42)'


# Comparison operators
lisp '(= 42 "abc")' | should_be 'Invalid type: expected number, found "abc"'
lisp '(= 2 "abc" 123)' | should_be 'Expected 2 args; found values (2 "abc" 123)'

lisp '(= 42 42)' | should_be '#t'
lisp '(= 42 0)' | should_be '#f'

lisp '(< 2 3)' | should_be '#t'
lisp '(< 3 2)' | should_be '#f'

lisp '(> 3 2)' | should_be '#t'
lisp '(> 2 3)' | should_be '#f'

lisp '(/= 42 42)' | should_be '#f'
lisp '(/= 42 0)' | should_be '#t'

lisp '(<= 2 3)' | should_be '#t'
lisp '(<= 2 2)' | should_be '#t'
lisp '(<= 3 2)' | should_be '#f'

lisp '(>= 3 2)' | should_be '#t'
lisp '(>= 3 3)' | should_be '#t'
lisp '(>= 2 3)' | should_be '#f'

lisp '(&& #t #t)' | should_be '#t'
lisp '(&& #t #f)' | should_be '#f'
lisp '(&& #f #t)' | should_be '#f'
lisp '(&& #f #f)' | should_be '#f'

lisp '(|| #t #t)' | should_be '#t'
lisp '(|| #t #f)' | should_be '#t'
lisp '(|| #f #t)' | should_be '#t'
lisp '(|| #f #f)' | should_be '#f'

lisp '(string=? "hello" "hello")' | should_be '#t'
lisp '(string=? "hello" "abc")' | should_be '#f'

lisp '(string<? "h" "hello")' | should_be '#t'
lisp '(string<? "hello" "h")' | should_be '#f'
lisp '(string<? "hello" "hello")' | should_be '#f'

lisp '(string>? "hello" "h")' | should_be '#t'
lisp '(string>? "h" "hello")' | should_be '#f'
lisp '(string>? "hello" "hello")' | should_be '#f'

lisp '(string<=? "h" "hello")' | should_be '#t'
lisp '(string<=? "hello" "h")' | should_be '#f'
lisp '(string<=? "hello" "hello")' | should_be '#t'

lisp '(string>=? "hello" "h")' | should_be '#t'
lisp '(string>=? "h" "hello")' | should_be '#f'
lisp '(string>=? "hello" "hello")' | should_be '#t'

lisp '(if (> 2 3) "yes" "no")' | should_be '"no"'
lisp '(if (< 2 3) "yes" "no")' | should_be '"yes"'
lisp '(if 1 2 3 4)' | should_be 'Expected 3 args; found values (1 2 3 4)'
lisp '(if 1 2)' | should_be 'Expected 3 args; found values (1 2)'

lisp "(car '(1 2 3))" | should_be '1'
lisp "(car '(1))" | should_be '1'
lisp "(car '(1 2 . 3))" | should_be '1'
lisp '(car "thing")' | should_be 'Invalid type: expected pair, found "thing"'
lisp '(car 1 2 3)' | should_be 'Expected 1 args; found values (1 2 3)'

lisp "(cdr '(a b c))" | should_be '(b c)'
lisp "(cdr '(a b))" | should_be '(b)'
lisp "(cdr '(a))" | should_be '()'
lisp "(cdr '(a . b))" | should_be 'b'
lisp "(cdr '(a b . c))" | should_be '(b . c)'
lisp "(cdr 'a)" | should_be 'Invalid type: expected pair, found a'
lisp "(cdr 'a 'b)" | should_be 'Expected 1 args; found values (a b)'

lisp "(cons 1 ())" | should_be '(1)'
lisp "(cons 1 '(2 3))" | should_be '(1 2 3)'
lisp "(cons 1 '(2 . 3))" | should_be '(1 2 . 3)'
lisp "(cons 1 2)" | should_be '(1 . 2)'
lisp "(cons '(1 2) 3)" | should_be '((1 2) . 3)'
lisp "(cons 1 2 3)" | should_be 'Expected 2 args; found values (1 2 3)'
lisp "(cons 1)" | should_be 'Expected 2 args; found values (1)'

lisp "(eqv? '(1 2 3) '(1 2 3))" | should_be '#t'
lisp '(eqv? 2 2)' | should_be '#t'
lisp '(eqv? 2 "2")' | should_be '#f'

lisp "(equal? '(1 2 3) '(1 2 3))" | should_be '#t'
lisp '(equal? 2 2)' | should_be '#t'
lisp '(equal? 2 "2")' | should_be '#t'
lisp '(equal? (quote 2) "2")' | should_be '#t'
lisp '(equal? (quote (1 "2")) (quote (1 2)))' | should_be '#t' # recursive weak typing
lisp '(equal? 2 3)' | should_be '#f'

#____________________________________________________________________________


# If we haven't exited yet then all tests must have passed
echo "All tests passed"
