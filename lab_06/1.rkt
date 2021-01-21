#lang scheme

(define (checkNumInList ls x)
	(define (iter counter)
		(cond
			{(and (= (- (length ls) 1) counter) (not (= x (list-ref ls counter)))) #f}
			{(= (length ls) counter) #t}
			{(= (list-ref ls counter) x) #t}
			{else (iter (+ counter 1))}
		)
	)
	(iter 0)
)
;(checkNumInList '(23 44 123 43) 43)
(define (countUniqueNums ls)
	(define (iter res counter)
		(cond
			{(= counter (- (length ls) 1)) (+ (length res) 1)}
			{(not (checkNumInList res (list-ref ls counter))) (iter (append res (list (list-ref ls counter))) (+ counter 1))}
			{else (iter res (+ counter 1))}
		)
	)
	(iter (list (list-ref ls 0)) 1)
)
(countUniqueNums '(1 3 3 3 2 4 5 7))