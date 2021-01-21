#lang scheme

(define (mod10 n)
  (remainder n 10)
)
(define (div10 n)
  (quotient n 10)
)
(define (minDigit x)
	(if (< x 10) x (if (> (mod10 x) (minDigit (div10 x))) (minDigit (div10 x)) (mod10 x)))
)
(define x 64423534)
(minDigit x)