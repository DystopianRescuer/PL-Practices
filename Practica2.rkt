#lang racket

(define (my-map f lst)
    (match lst
        ['() lst]
        [(cons x xs) (cons (f x) (my-map f xs))]))

(define (my-filter f lst)
    (match lst
        ['() lst]
        [(cons x xs)
            (if (f x)
                (cons x (my-filter f xs))
                (my-filter f xs ))]))

(define (a-binario lst) (my-map reverse (my-map binario-de lst)))

; Toma un numero y te regresa una lista de sus digitos en binario, pero en little-endian
(define (binario-de n) (if (equal? n 0)
    empty
    (cons (modulo n 2) (binario-de (quotient n 2)))))
