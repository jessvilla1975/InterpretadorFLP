#lang racket/base

(require rackunit "../archivo.rkt")

;test 1
(define exp1 
  (scan&parse
    "let
      x = 2
      in
        if (x > 1) {
          3
        else 
          4
        }
    "
  )
)

;test 2
(define exp2 
  (scan&parse
    "let
      x = 2
      in
        if (x < 1) {
          3
        else 
          4
        }
    "
  )
)

(define exp3
  (scan&parse
    "let
      x = 2
      y = 2
      in
        if (x == y) {
          10
        else 
          20
        }
    "
  )
)

(define exp4
  (scan&parse
    "let
      x = 3
      y = 2
      in
        if (x != y) {
          50
        else 
          20
        }
    "
  )
)

(define exp5
  (scan&parse
    "let
      x = 3
      y = 2
      in
        if (x >= y) {
          120
        else 
          5
        }
    "
  )
)



(check-equal? (eval-program exp1) 3)
(check-equal? (eval-program exp2) 4)
(check-equal? (eval-program exp3) 10)
(check-equal? (eval-program exp4) 50)
(check-equal? (eval-program exp5) 120)




