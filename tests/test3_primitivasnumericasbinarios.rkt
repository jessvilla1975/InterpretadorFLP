#lang racket/base

(require rackunit "../archivo.rkt")

;(check-equal? (sumar-binarios "b1000" "b10") "b1010")
;(check-equal? (sumar-binarios "b0011" "b0011") "b110")

;test 1
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

;test 2
(define exp2 
  (scan&parse
    "
    let
    x = b1000
    y = b10
    in
      (x - y)
    "
  )
)
(define expected-exp2
  "b110"
)

;test 2
(define exp3
  (scan&parse
    "
    let
    x = b10
    y = b10
    in
      (x * y)
    "
  )
)
(define expected-exp3
  "b100"
)

(check-equal? (eval-program exp1) expected-exp1)
(check-equal? (eval-program exp2) expected-exp2)
(check-equal? (eval-program exp3) expected-exp3)








