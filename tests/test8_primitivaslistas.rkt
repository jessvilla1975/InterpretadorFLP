#lang racket/base

(require rackunit "../archivo.rkt")


;test 1
(define exp1 
  (scan&parse
    "
    let
    l = cons(1 cons(2 cons(3 empty)))
    in
      list(first(l), rest(l), empty?(rest(l)))
    "
  )
)
  
(check-equal? (eval-program exp1) '(1 (2 3) #f))







