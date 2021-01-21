#lang scheme
; Задание 4
(define (chessPieces X Y x y)
	(cond
		{(= (remainder (+ X Y) 2) (remainder (+ x y) 2)) (if (not (= (abs (- X x)) (abs (- Y y)))) 2 1)}
		{else 0}
	)
)
(define-values (X Y x y) (values (read ) (read ) (read ) (read )))
(display (chessPieces X Y x y))