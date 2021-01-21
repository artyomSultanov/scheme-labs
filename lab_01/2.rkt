#lang scheme
; Задание 2
(define (calcAng h m) 
	(abs (- (+ (* h 30) (/ m 2)) (* m 6)))
)
(define (getAng h m)
	(cond 
		{(> h 12) (calcAng (- h 12) m)}
		{else (calcAng h m)}
	)
)
(define h (read ))
(define m (read ))
(getAng h m)