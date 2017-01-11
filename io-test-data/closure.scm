(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(define my-count (counter 5))
(my-count 3)
