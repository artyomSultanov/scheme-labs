#lang scheme

(define (lenNum n)
	(if (< n 10)
		1
		(+ 1 (lenNum (quotient n 10)))
	)
)

(define (myReverse x) 
	(define (iter elem counter len num)
		(if (number? x)
			(cond 
				{(= len counter) elem}
				{else (iter (+ (* elem 10) (remainder num 10)) (+ counter 1) len (quotient num 10))}
			)
			(cond
				{(= len (- 1)) elem}
				{else (iter (string-append elem (string (string-ref x len))) counter (- len 1) num)}
			)
		)
	)
	(if (number? x)
		(iter 0 0 (lenNum x) x)
		(iter "" 0 (- (string-length x) 1) 0)
	)
)
(define (clone? x y)
	(define (iter res counter revCounter)
		(cond
			{(not (= (length x) (length y))) #f}
			{(and (= counter (- (length x) 1)) (= (list-ref x counter) (myReverse (list-ref y revCounter)))) #t}
			{(not (= (list-ref x counter) (myReverse (list-ref y revCounter)))) #f}
			{else (iter #f (+ counter 1) (- revCounter 1))}
		)
	)
	(iter #f 0 (- (length x) 1))
)
(clone? '(1 23 456) '(654 32 1))