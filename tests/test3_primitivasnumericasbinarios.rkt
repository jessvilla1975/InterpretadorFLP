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

(define exp4
  (scan&parse
    "
    let
    x = b10
    y = b011
    in
      (x pow y)
    "
  )
)

(define exp5
  (scan&parse
    "
    let
    x = b101
    y = b011
    in
      (x mod y)
    "
  )
)

(check-equal? (eval-program exp1) "b10110")
(check-equal? (eval-program exp2) "b110")
(check-equal? (eval-program exp3) "b100")
(check-equal? (eval-program exp4) "b1000")
(check-equal? (eval-program exp5) "b10")







