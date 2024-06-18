#lang racket/base

(require rackunit "../archivo.rkt")


(define exp1 
  (scan&parse
    "
    let
    x = b1010
    y = b1100
    in
      (x + y)
    "
  )
)
(define expected-exp1
  "b10110"
)



(check-equal? (eval-program exp1) expected-exp1)











