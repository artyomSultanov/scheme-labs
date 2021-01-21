#lang scheme

(define (newListUni ls)
	(define (iter newls counter)
		(cond
			{(= (length ls) counter) newls}
			{(= (remainder (list-ref ls counter) (+ counter 1)) 0) (iter (append newls (list (list-ref ls counter))) (+ counter 1))}
			{else (iter newls (+ counter 1))}
		)
	)
	(iter '() 0)
)
(newListUni '(1 2 2 4))