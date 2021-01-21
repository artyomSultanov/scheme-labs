#lang scheme
; Задание 3
(define namelistOfDigits (list "Ноль" "Один" "Два" "Три" "Четыре" "Пять" "Шесть" "Семь" "Восемь" "Девять"))
(define namelistOfNums (list "Сто" "Десять" "Двадцать" "Тридцать" "Сорок" "Пятьдесят" "Шестьдесят" "Семьдесят" "Восемьдесят" "Девяносто"))
(define namelistOfTenToTwenty (list "Десять" "Одиннадцать" "Двенадцать" "Тринадцать" "Четырнадцать" "Пятнадцать" "Шестандцать" "Семнадцать" "Восемнадцать" "Девятнадцать"))
(define (num2word x)
  (cond
  	{(= x 0) (list-ref namelistOfDigits 0)}
  	{(= x 100) (list-ref namelistOfNums 0)}
  	{(= (quotient x 10) 1) (list-ref namelistOfTenToTwenty (remainder x 10))}
  	{(= x 12) (list "")}
  	{else (list (if (= (quotient x 10) 0) "" (list-ref namelistOfNums (quotient x 10))) (if (= (remainder x 10) 0) "" (list-ref namelistOfDigits (remainder x 10))))}
  )
)
(define x (read ))
(display (num2word x))