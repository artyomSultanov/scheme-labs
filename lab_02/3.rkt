#lang scheme

(define (Fib x y)
	(define (Fib-iter a b k)
		(display (if (= b y) #t ""))
		(if (= k 0)
			b
			(Fib-iter b (+ a b) (- k 1))
		)
	)
	(Fib-iter 1 1 (- x 1))
)

(define x 11)
(Fib x x)