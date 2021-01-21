#lang scheme
(define (prime? x) ; Проверяет, является ли число простым
	(define (iter k)
		(if (< k 2)
			#t
			(if (= (remainder x k) 0)
				#f
				(iter (- k 1))
			)
		)
	)
	(iter (floor (sqrt x)))
)
(define (takeNextPrime x lim)
	(if (and (prime? x) (<= x lim)) x (takeNextPrime (+ x 1) lim))
)
(define (factorization num)
	(define (iter ls x prime counter)
		(cond 
			{(<= x 1) (append ls (list (cons prime counter)))}
			{(= (remainder x prime) 0) (iter ls (/ x prime) prime (+ counter 1))}
			{(and (not (= (remainder x prime) 0)) (= counter 0)) (iter ls x (takeNextPrime (+ prime 1) num) counter)}
			{(not (= (remainder x prime) 0)) (iter (append ls (list (cons prime counter))) x (takeNextPrime (+ prime 1) num) 0)}
		)
	)
	(iter '() num 2 0)
)
(factorization 12)