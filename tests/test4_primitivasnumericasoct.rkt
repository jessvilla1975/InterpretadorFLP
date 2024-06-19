#lang racket/base

(require rackunit "../archivo.rkt")


;test 1

(define exp1
  (scan&parse
    "
    let
    x =  hx100
    y =  hx2
    in
      (x - y)
    "
  )
)

  
(check-equal? (eval-program exp1) "hxFE")







