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


; tets 2
(define exp2
  (scan&parse
    "
    or((3 < 5), true)
    "
  )
)


; test 3
(define exp3
  (scan&parse
    "
    not(true)
    "
  )
)


; test 4
(define exp4
  (scan&parse
    "
    xor(true, false)
    "
  )
)

(check-equal? (eval-program exp1) #f)
(check-equal? (eval-program exp2) #t)
(check-equal? (eval-program exp3) #f)
(check-equal? (eval-program exp4) #t)






