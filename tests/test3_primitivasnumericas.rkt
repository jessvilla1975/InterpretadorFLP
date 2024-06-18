#lang racket/base

(require rackunit "../archivo.rkt")

;***************decimales*************
;test 1: 
(define exp1 
  (scan&parse
    "
    let
    x = 10
    y = 10
    in
      (x + y)
    "
  )
)
(define expected-exp1
  20
)

;test 2: 
(define exp2
  (scan&parse
    "
    let
    x = 20
    y = 10
    in
      (x - y)
    "
  )
)
(define expected-exp2
  10
)

; test 3
(define exp3
  (scan&parse
    "
    let
    x = 20
    y = 10
    in
      (x * y)
    "
  )
)
(define expected-exp3
  200
)

; tets 4
(define exp4
  (scan&parse
    "
    let
    x = -20
    y = 10
    in
      (x + y)
    "
  )
)
(define expected-exp4
  -10
)
;**************flotante******************
;test 5
(define exp5
  (scan&parse
    "
    let
    x = 10.5
    y = 10.5
    in
      (x + y)
    "
  )
)
(define expected-exp5
  21.0
)
;test 6
(define exp6
  (scan&parse
    "
    let
    x = 20.5
    y = 10.5
    in
      (x - y)
    "
  )
)
(define expected-exp6
  10.0
)

;**************binarios*************

; test 7





(check-equal? (eval-program exp1) expected-exp1)
(check-equal? (eval-program exp2) expected-exp2)
(check-equal? (eval-program exp3) expected-exp3)
(check-equal? (eval-program exp4) expected-exp4)
(check-equal? (eval-program exp5) expected-exp5)
(check-equal? (eval-program exp6) expected-exp6)











