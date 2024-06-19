#lang racket/base

(require rackunit "../archivo.rkt")


;test 1

(define exp1
  (scan&parse
    "
    let
    x = 0x77
    y = 0x3
    in
      (x + y)
    "
  )
)
(define exp2
  (scan&parse
    "
    let
    x = 0x144
    y = 0x24
    in
      (x - y)
    "
  )
)
  
(check-equal? (eval-program exp1) "0x102")
(check-equal? (eval-program exp2) "0x120")






