#lang scheme
; Задание 1
(define (deadarea x y a)
	(cond
		{(and (<= y 0) (<= (abs x) a) (<= (abs y) a)) (display 0)}
		{(>= y 0) (display (- (sqrt (+ (* x x) (* y y))) a))}
		{(< y 0) (display (- (abs (min x y)) a))}
		{else (display #f)}
	)
)
(define x (read ))
(define y (read ))
(define a (read ))
(deadarea x y a)