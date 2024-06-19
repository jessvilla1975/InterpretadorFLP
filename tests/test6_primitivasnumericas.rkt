#lang racket/base

(require rackunit "../archivo.rkt")


;test 1

(define exp1
  (scan&parse
    "
    let
    x = 2
    y = 2
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
    x = 2.5
    y = 2.5
    in
      (x + y)
    "
  )
)
  
(check-equal? (eval-program exp1) 4)
(check-equal? (eval-program exp2) 5.0)






