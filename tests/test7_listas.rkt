#lang racket/base

(require rackunit "../archivo.rkt")

; test 1: lista
(define exp1
  (scan&parse
    "
    list(1, 2, 3, 4)
    "
  )
)

; test 2: cons
(define exp2
  (scan&parse
    "
    cons(1 cons(2 empty))
    "
  )
)

; test 3: empty
(define exp3
  (scan&parse
    "
    empty
    "
  )
)
(check-equal? (eval-program exp1) '(1 2 3 4))
(check-equal? (eval-program exp2) '(1 2))
(check-equal? (eval-program exp3) '())
