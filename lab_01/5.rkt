#lang scheme
; Задание 5
(define (chessPieces2 x y X Y)
	(cond 
		{(or (> (abs (- X x)) y) (> Y (+ y 1)) ) #f}
		{else #t}
	)
)
(define-values (x y X Y) (values (read ) (read ) (read ) (read )))
(display (chessPieces2 x y X Y))