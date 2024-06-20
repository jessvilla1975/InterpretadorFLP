#lang racket/base

(require rackunit "../archivo.rkt")


(define exp1
  (scan&parse
    "var
      x = 1
      in
        begin
          set x = 2;
          x
        end
    "
  )
)


(check-equal? (eval-program exp1) 2)

