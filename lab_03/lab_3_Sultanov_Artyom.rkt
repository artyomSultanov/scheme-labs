#lang scheme

; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
(define (mod10 n)
  (remainder n 10)
)
(define (div10 n)
  (quotient n 10)
)
(define (lenNum n) ; выводи длину числа
	(if (< n 10)
		1
		(+ 1 (lenNum (div10 n)))
	)
)

; ---------------------------------------
; Задача 1
(define (DisDivNum x) ; проверяет, делится ли число на свои цифры. Если да, то +1, иначе +0. Выводит сумму (за каждую цифру +1)
	(define (ddn-iter k y)
		(if (= y 0)
			0
			(if (= (mod10 y) 0)
				(ddn-iter k (div10 y))
				(if (= (remainder k (mod10 y)) 0)
					(+ 1 (ddn-iter k (div10 y)))
					0
				)
			)
		)
	)
	(ddn-iter x x)
)
;(DisDivNum 36)

(define (checkDDN x) ; Сравнивает {значение из функции DisDivNum} с длиной данного числа 
	(if (= (DisDivNum x) (lenNum x)) #t #f)
)
;(checkDDN 48) 

; --------------------------------------
; Задача 2
(define (log2 x) ; Находит логарифм числа х по основанию 2
	(define (iter num pow res)
		(if (>= res num)
			(if (= res num) pow #f)
			(iter num (+ pow 1) (* 2 res))
		)
	)
	(iter x 0 1)
)
;(log2 1024)

; --------------------------------------
; Задача 3
(define (SyrSeq x a0) ; Генерирует и выводит первые х членов "сиракузской последовательности"
	(define (iter i term seq)
		(if (= i 0)
			(iter (+ i 1) (+ term (+ a0 1)) (append seq (list a0)))

			(if (= i x)
				seq
				(if (= (remainder term 2) 0)
					(iter (+ i 1) (+ term 1) (append seq (list (/ term 2))))
					(iter (+ i 1) (+ term 1) (append seq (list (+ (* term 3) 1))))
				)
			)
		)
	)
	(iter 0 0 '())
)
;(SyrSeq 5 0)

; -------------------------------------
; Задача 4
(define (prime? x) ; Проверяет, является ли число простым
	(define (iter k)
		(if (< k 2)
			#t
			(if (= (remainder x k) 0)
				#f
				(iter (- k 1))
			)
		)
	)
	(iter (floor (sqrt x)))
)
;(prime? 11)

(define (prodPrimes? x) ; Проверяет, является ли число - произведением двух простых
	(define (iter k quo terms)
		(if (< k 2) 
			#f
			(if (and (= quo 1) (= terms 2))
				#t
				(if (and (prime? k) (= (remainder quo k) 0))
					(iter k (/ quo k) (+ terms 1))
					(iter (- k 1) quo terms)
				)
			)
		)
	)
	(iter (- x 1) x 0)
)
;(prodPrimes? 10)

; -----------------------------------
; Задача 5
;(define (lenNum n)
;	(if (< n 10)
;		1
;		(+ 1 (lenNum (quotient n 10)))
;	)
;)

;(define (inCell? x)
;	(define (iter k)
;		(if (< x 10) 
;			x
;			; to be continued...
;		)
;	)
;	(iter (if (< x 10) x (- x 9)))
;)
;(inCell? 15)