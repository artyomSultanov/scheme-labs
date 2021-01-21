#lang scheme

(define (maxNumsInQueue ls)
	(define (iter maxQ numCount lastNum par counter)
		(cond
			{(= (length ls) counter) maxQ}
			{(= (- lastNum (list-ref ls counter)) 2) (iter maxQ (+ numCount 1) (list-ref ls counter) par (+ counter 1))}
			{(not (= (- lastNum (list-ref ls counter)) 2)) (iter (if (< maxQ numCount) numCount maxQ) 1 (list-ref ls counter) (even? (list-ref ls counter)) (+ counter 1))}
		)
	)
	(iter 1 1 (list-ref ls 0) (even? (list-ref ls 0)) 0)
)
(maxNumsInQueue '(1 3 4 6 8 5 7 9 11 13 2 4))