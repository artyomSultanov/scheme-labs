#lang scheme

(define (Fib x y)
	(define (Fib-iter a b k)
		(if (= k 0)
			b
			(if (< (abs (- b x)) (abs (- x (Fib-iter b (+ a b) (- k 1))))) 
				b
				(Fib-iter b (+ a b) (- k 1))
			)
		)
	)
	(Fib-iter 1 1 (- x 1))
)
(define x 11)
(Fib x x)