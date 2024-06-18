#lang racket/base

(require rackunit "../archivo.rkt")

;test 1
(define exp1 
  (scan&parse
    "let
      x = 2
      in
        if (x > 1) {
          3
        else 
          4
        }
    "
  )
)
(define expected-exp1
  3
)

;test 2
(define exp2 
  (scan&parse
    "let
      x = 2
      in
        if (x < 1) {
          3
        else 
          4
        }
    "
  )
)

(check-equal? (eval-program exp1) expected-exp1)
(check-equal? (eval-program exp2) 4)






