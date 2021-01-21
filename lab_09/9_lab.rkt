#lang racket
; Задание 1. Выводит строку с наибольшим количеством нулей
(define (counterZeros lst)
	(define (iter lst counter)
		(cond
			{(empty? lst) counter}
			{(equal? 0 (car lst)) (iter (cdr lst) (+ 1 counter))}
			{else (iter (cdr lst) counter)}
		)
	)
	(iter lst 0)
)
;(counterZeros '(1 0 1 0 0 2 3 0))
(define (Lab09_1 Mat)
	(define (iter counter max)
		(cond
			{(or (empty? Mat) (= max 0)) null}
			{(= max (counterZeros (list-ref Mat counter))) (list-ref Mat counter)}
			{else (iter (add1 counter) max)}
		)
	)
	(iter 0 (argmax add1 (map (lambda (row) (counterZeros row)) Mat)))
)
;(Lab09_1 '((0 1 1) (0 0 1) (1 0 1))) ;'(0 0 1)

; Задание 2. Выводит матрицу по принципу в условии
(define (buildStrMat n indent) ; создает строку по принципу в условии
	(build-list n (lambda (x) 
	(cond 
		{(or (< x indent) (>= x (- n indent))) 1}
		{else 0}
	)))
)
;(buildStrMat 5 2) ;'(1 1 0 1 1)

(define (Lab09_2 n)
	(define (iter Mat borderCols)
		(build-list borderCols (lambda (x)
			(cond
				{(>= x (- borderCols 1)) (append (buildStrMat n (add1 x)) Mat) }
				{else (append (buildStrMat n (add1 x)) Mat)}
			)))
	)
	(define result (iter null (if (odd? n) (+ 1 (floor (/ n 2))) (/ n 2))))
	(if (odd? n) 
		(append result (cdr (reverse result)))
		(append result (reverse result))
	)
)
;(Lab09_2 3) ;'((1 0 1) (1 1 1) (1 0 1))
