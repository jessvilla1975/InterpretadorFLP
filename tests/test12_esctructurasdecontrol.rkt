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
;test3
(define exp3
  (scan&parse
    "let
      x = 0
      in
        begin
          while (x < 10) {
            set x = (x + 1)
          };
          x
        end
    "
  )
)

;test 4
(define exp4
  (scan&parse
    "
    let
      x = 0
    in
      begin
        switch (x) {
          case 1 : 1
          case 2 : 2
          default : 3
        }
      end
    "
  )
)


(check-equal? (eval-program exp1) 45)
(check-equal? (eval-program exp2) 20)
(check-equal? (eval-program exp3) 10)
(check-equal? (eval-program exp4) 3)