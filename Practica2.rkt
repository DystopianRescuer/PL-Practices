#lang racket

; --------------- implementaciones vistas en clase ---------------
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
; ----------------------------------------------------------------


; Ejercicio 1
; Toma la lista, convierte a binario cada elemento, y pasa de little-endian a big-endian
(define (a-binario lst) (my-map reverse (my-map (letrec
    ([binario-de (lambda (n) (if (equal? n 0)
        empty
        (cons (modulo n 2) (binario-de (quotient n 2)))))])
    (binario-de lst)))))
