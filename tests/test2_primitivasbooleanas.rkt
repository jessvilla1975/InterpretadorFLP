#lang racket/base

(require rackunit "../archivo.rkt")

;test 1

(define exp1
  (scan&parse
    "
    and((1 > 2), false)
    "
  )
)
(define expected-exp1
  #f
)

; tets 2
(define exp2
  (scan&parse
    "
    or((3 < 5), true)
    "
  )
)
(define expected-exp2
  #t
)

; test 3
(define exp3
  (scan&parse
    "
    not(true)
    "
  )
)
(define expected-exp3
  #f
)

; test 4
(define exp4
  (scan&parse
    "
    xor(true, false)
    "
  )
)
(define expected-exp4
  #t
)


(check-equal? (eval-program exp1) expected-exp1)
(check-equal? (eval-program exp2) expected-exp2)
(check-equal? (eval-program exp3) expected-exp3)
(check-equal? (eval-program exp4) expected-exp4)






