#lang scheme
;1 Суммирование матриц
(define (sumMats Mat1 Mat2)
	(map (lambda (r1 r2) (map + r1 r2)) Mat1 Mat2)
)
;(sumMats '((1 1 1) (2 2 2) (3 3 3)) '((3 3 3) (2 2 2) (1 1 1)))

;2 Проверка матрицы на отрицательное число
(define (posMat? Mat)
	(andmap (lambda (row) (andmap positive? row)) Mat)
)
;(posMat? '((1 2 3) (-1 2 3)))

;3 Проверка на нижнетреугольную матрицу
(define (low3angle Mat)
	(if 
		(foldl (lambda (row i) 
			(if i
				(if (andmap zero? (take row (- i 1)))
					(+ i 1)
					#f
				)
				#f
			)
		) 1 Mat)
		#t
		#f
	)
)
;(low3angle '((1 1 1) (0 1 1) (0 0 1)))