#lang racket

;1 Скалярное произведение
(define (ScPr1 lst1 lst2)
  (foldl + 0 (map * lst1 lst2))
)

(define (ScPr2 v1 v2)
  (foldl (λ(x y s)(+ s (* x y))) 0 v1 v2)
)

;2 Список минимальных цифр

(define (MinDigList lst)

  (define (MinDig n)
    (if (< n 10) n
        (min (remainder n 10) (MinDig (quotient n 10)))
    )
  )

  (map MinDig lst)
)

;3 Список квадратов элементов

(define (ListOfSquares1 lst)
  (map (λ(x)(sqr x)) lst)
)


;4 Список остатков от деления n на i

(define (Rems n)
  (build-list n (λ(x)(remainder n (+ x 1))))
)

;5 Сиракузская последовательность

(define (Siracusa a0 n)
  (reverse (foldl (λ(x s)(if (even? (car s)) (cons (quotient (car s) 2) s) (cons (+ (* 3 (car s)) 1) s))) (list a0) (build-list (- n 1) values)))
)

;6 Есть хотя бы один ноль в списке

(define (IncludesZero? lst)
  (ormap (λ(x)(eq? x 0)) lst)
)

(define (IncludesZero2? lst)
  (list? (member 0 lst))
)

;7 Все элементы списка разные?
;Три версии

(define (diff1 lst)
  (andmap (λ(x) (member x (cdr (member x lst)))) lst))

(define (diff2 lst)
  (equal? lst (remove-duplicates lst)))

(define (diff3 lst)
  (list? (foldl (λ(x res)
           (if res
               (if (member x res) #f (cons x res))
               res))
         '() lst)))

;8 Подсписки ((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)...)

(define (SubLists lst)
  (reverse (foldl (λ(x s)(cons (take lst (+ x 1)) s)) '() (build-list (length lst) values)))
)