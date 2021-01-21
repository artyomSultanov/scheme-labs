#lang scheme

(define (mod10 n)
  (remainder n 10)
)
(define (div10 n)
  (quotient n 10)
)

(define (EqDigits x)
	(if (< x 10) x (if (= (mod10 x) (EqDigits (div10 x))) (EqDigits (div10 x)) (- 1)))
)

(define x 2222)
(define y 3133)

(positive? (EqDigits x))
(positive? (EqDigits y))