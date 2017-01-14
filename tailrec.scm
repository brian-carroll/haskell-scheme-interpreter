(define (addtailrec x total) (if (= x 0) total (addtailrec (- x 1) (+ total 1))))
(addtailrec 5 0)
