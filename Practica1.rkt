#lang racket

; ------------------ Ejercicio 7 -------------------------
; Una funci ́on recursiva que rota una lista hacia la izquierda un n ́umero de veces dado, recibe una lista lst y un n ́umero para rotar n, devuelve la lista ya rotada.
(define (rotate-list lst n)
    (if (> n 0)
    (rotate-list (rota-izq lst) (- n 1))
    lst))

; Función que rota a la izquierda una lista una vez
(define (rota-izq list)
    (if (< (length list) 2)
        list
        (cons (second list) (rota-izq (cons (first list) (rest (rest list)))))))


; ---------------- Ejercicio 8 ---------------------------------
; zigzag-sum: Una función recursiva recibe una lista de números lst y devuelve la suma de los elementos alternando entre suma y restar cada elemento.
(define (zigzag-sum lst) (if (< (length lst) 2)
    (if (equal? (length lst) 1) (last lst) 0)
    (+ (- (first lst) (second lst))
        (zigzag-sum (rest (rest lst))))))


; ----------------- Ejercicio 9 -------------------------------
; generate-brackets: Una funci ́on recursiva recibe un n ́umero n y devuelve una lista de todas las combinaciones v ́alidas de par ́entesis para n pares.
;(define (generate-brackets n))


; ----------------- Ejercicio 10 ---------------------------
; count-steps-to-one: Una función recursiva recibe un n ́umero entero n y devuelve el n ́umero de pasos necesarios para reducir el n ́umero dado a 1 siguiendo estas reglas:
; Si es par, div ́ıdelo en 2.
; Si es impar, multipl ́ıcalo por 3 y s ́umale 1.
(define (count-steps-to-one n)
    (if (equal? n 1) 0
        (+ 1 (count-steps-to-one (if (even? n) (/ n 2) (+ (* n 3) 1))))))
