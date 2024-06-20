#lang racket/base

(require rackunit "../archivo.rkt")

;test 1
(define exp1 
  (scan&parse
    "var
      x = 0
      in
        begin
          for i from 0 until 10 by 1 do
            set x = (x + i);
          x
        end
    "
  )
)
;test 2
(define exp2
  (scan&parse
    "
    var
      x = 0
      in
        begin
          for i from 0 until 10 by 2 do
            set x = (x + i);
          x
        end
    "
  )
)

(check-equal? (eval-program exp1) 45)
(check-equal? (eval-program exp2) 20)
