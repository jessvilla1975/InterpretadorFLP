#lang racket/base

(require rackunit "../archivo.rkt")


;test 1
(define exp1 
  (scan&parse
    "
    let
    s = array(1,2,3,4,5)
    in
      length(s)
    "
  )
)

;test 2
(define exp2
  (scan&parse
    "
    let
    s = array(1,2,3,4,5)
    in
      index(s,2)
    "
  )
)

;test 3

(define exp3
  (scan&parse
    "
    let
    k = array(1, 2, 3, 4, 5)
    in
      setlist(k, 2 , 10)
    "
  )
)


(check-equal? (eval-program exp1) 5)
(check-equal? (eval-program exp2) 3)
(check-equal? (eval-program exp3) '#(1 2 10 4 5)) ;corregir parte del codigo

