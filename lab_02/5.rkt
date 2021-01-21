#lang scheme

; Выводит максимальный делитель числа (вторым аргументом можно передать этот же максимальный делитель, чтобы исключить его, из-за чего
; будет выведен следущий максимальный делитель)
(define (chechDivider x z)
	(define (div-iter t)
		(if (> t 0)
			(if (= (remainder x t) 0) t (div-iter (- t 1)))
			0
		)
	)
	(div-iter (- x (+ (- x z) 1)))
)
(chechDivider 24 12) ; 8

; считает сумму делителей
(define (sumDivs n y)
	(if (> y 1)
		(+ (chechDivider n y) (sumDivs n (chechDivider n y)))
		0
	)

)

; проверяет на совершенность
(define (perfect? n y)
	(if (= n (sumDivs n n)) #t #f)
)
(perfect? 6 6) ; #t