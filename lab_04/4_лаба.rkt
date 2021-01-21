#lang scheme
; Общие вспомогательные функции
(define (lenNum n) ; Выводит длину числа <- Что делает функция
	(if (< n 10)
		1
		(+ 1 (lenNum (quotient n 10)))
	)
) 
;(lenNum 125) ; 3 <- Пример работы: [Вызов функции] (разделение комментариев для удобной проверки вызова) [Вывод функции]

; Задание 1 --------------------------
(define (concatenateNums first last) ; Соединяет два числа и ставит между ними точку
	(string-append 
		(number->string first)
		(string-append "." (number->string last))
	)
) 
;(concatenateNums 123 456) ; "123.456"

(define (separate x k) ; Главная функция. Разделяет число {x} на две части точкой. В правой части количество элементов {k}
	(if (<= (lenNum x) k) 
		(concatenateNums 0 x) 
		(if (= k 0)
			(concatenateNums x 0)
			(string-append 
				(substring (number->string x) 0 (- (lenNum x) k))
				(string-append "." (substring (number->string x) (- (lenNum x) k)))
			)
		)
	)
)
;(separate 12345 3) ; "12.345"


; Задание 2 --------------------------
(define (allEven? x) ; Проверяет, все ли элементы в списке {x} четные. Сначала считает кол-во счётных, потом сравнивает со длиной списка
	(define (iter k count)
		(cond
			{(empty? x) (- 1)}
			{(= k (length x)) count}
			{(= (remainder (list-ref x k) 2) 0) (iter (+ k 1) (+ count 1))}
			{(= (remainder (list-ref x k) 2) 1) (iter (+ k 1) count)}
			{else (- 1)}
		)
	)
	(if (= (iter 0 0) (length x)) #t #f)
)
;(allEven? '(2 4 6 8)) ; #t


; Задание 3 --------------------------
(define (lookAround x) ; Выводит первое попавшееся число в списке {x}, соседи которого меньше этого числа
	(define (iter k)
		(cond 
			{(empty? x) '()}
			{(and (= k 0) (> (length x) 1) (> (list-ref x k) (list-ref x (+ k 1)))) (list-ref x k)}
			{(and (= k 0) (> (length x) 1) (< (list-ref x k) (list-ref x (+ k 1)))) (iter (+ k 1))}
			{(and (= k (- (length x) 1)) (< (list-ref x (- k 1)) (list-ref x k))) (list-ref x k)}
			{(and (< (list-ref x (- k 1)) (list-ref x k)) (< (list-ref x (+ k 1)) (list-ref x k))) (list-ref x k)}
			{(or (>= (list-ref x (- k 1)) (list-ref x k)) (>= (list-ref x (+ k 1)) (list-ref x k))) (iter (+ k 1))}
			{else '()}
		)
	)
	(iter 0)
)
;(lookAround '(6 7 7 8 5 9)) ; 8


; Задание 4 --------------------------
(define (reflect x) ; Отражает список {x} относительно первого элемента
	(define (iter newlist k side)
		(cond
			{(empty? x) '()}
			{(= (length x) 1) x}
			{(and side (> k 0)) (iter (append newlist (list (list-ref x k))) (- k 1) #t)}
			{(and side (= k 0)) (iter (append newlist (list (list-ref x k))) (+ k 1) #f)}
			{(and (not side) (= k (- (length x) 1))) (append newlist (list (list-ref x k)))}
			{(and (not side) (> k 0)) (iter (append newlist (list (list-ref x k))) (+ k 1) #f)}
			{else '()}
		)
	)
	(iter '() (- (length x) 1) #t)
)
;(reflect '(1 2 3)) ; (3 2 1 2 3)


; Задание 5 --------------------------
(define (prodElems x digit) ; Выдает произведение всех чисел {prod} в списке {x}, разряд которых меньше заданного {digit}
	(define (iter k prod)
		(cond
			{(empty? x) 0}
			{(and (<= (length x) 1) (> (lenNum (list-ref x 0)) digit)) 0}
			{(and (<= (length x) 1) (<= (lenNum (list-ref x 0)) digit)) (list-ref x 0)}
			{(< k 0) prod}
			{(<= (lenNum (list-ref x k)) digit) (iter (- k 1) (* prod (list-ref x k)))}
			{else (iter (- k 1) prod)}
		)
	)
	(iter (- (length x) 1) 1)
)
;(prodElems '(5 20 7 333) 2) ; 700