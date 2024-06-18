#lang racket/base

(require rackunit "../archivo.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "let
      a = 5
      in
        if (a > 3) {
          1
        else 
          2
        }
    "
  )
)
(define expected-exp1
  1
)

(check-equal? (eval-program exp1) expected-exp1)






