#lang racket/base

(require rackunit "../archivo.rkt")


;test 1
(define exp1 
  (scan&parse
    "
    let
    s = \"hola mundo cruel\"
    in
      string-length(s)
    "
  )
)
; test 2
(define exp2
  (scan&parse
    "
    let
    a = \"hola\"
    b = \"mundo\"
    c = \"cruel\"
    d = \"con\"
    e = \"flp\"
    in
      concat(a, b, c, d, e)
    "
  )
)
(define exp3
  (scan&parse
    "
    let
    s = \"hola mundo cruel\"
    in
      elementAt(s, 5)
    "
  )
)

(check-equal? (eval-program exp1) 16)
(check-equal? (eval-program exp2) "holamundocruelconflp")
(check-equal? (eval-program exp3) "m")





