#lang racket

; ------------------ Ejercicio 1 -------------------------
(define (balanceado? expr)
  (if (= 0 (balanceador-rec expr 0)) #t #f )
)

;;Funcion recursiva auxiliar que recibe una cadena y un acumulador que se incia en 0
(define(balanceador-rec cadena acumulador)
  (if (= 0(string-length cadena)) acumulador
      (if (equal? "(" (substring cadena 0 1))
          (balanceador-rec (substring cadena 1 (string-length cadena)) (+ acumulador 1))
          (if (> acumulador 0)
              (balanceador-rec (substring cadena 1 (string-length cadena)) (- acumulador 1))
              1
          )
      )
   )
)

; ------------------ Ejercicio 2 -------------------------
( define (vocal-predominante? s )
   (vocal-predominante?-rec s 0 0)
)

( define ( vocal-predominante?-rec s vocales consonantes)
   (if (equal? s "") (> vocales consonantes)
       (if (or (equal? (substring s 0 1) "a")
               (equal? (substring s 0 1) "e")
               (equal? (substring s 0 1) "i")
               (equal? (substring s 0 1) "o")
               (equal? (substring s 0 1) "u")
               (equal? (substring s 0 1) "A")
               (equal? (substring s 0 1) "E")
               (equal? (substring s 0 1) "I")
               (equal? (substring s 0 1) "O")
               (equal? (substring s 0 1) "U")
           )
           (vocal-predominante?-rec (substring s 1 (string-length s))
           (add1 vocales) consonantes )
           (vocal-predominante?-rec (substring s 1 (string-length s))
           vocales (add1 consonantes) )
       )
   )
)

; ------------------ Ejercicio 3 -------------------------
;Funcíon que determina si se puede establecer un ismorfismo de letras entre 2 cadenas
;devuelve true si si, false en otro caso
(define (cadena-isomorfa? s1 s2 )
   (if (not (= (string-length s1) (string-length s2))) #f
      (and (haymorfismo? s1 s2 '() (make-hash)) (haymorfismo? s2 s1 '() (make-hash)))
  )
)
;Funcion que devuelve true si se puede establecer un morfismo entre las letras de 2 strings
;recorre los 2 strings recursivamente
;-cuando no existe un valor en añadidos (añadidos es una lista que almacena las claves)
; lo agrega a añadidos y lo asocia con la letra del otro string al diccionario
;-si una clave ya existe la consulta, si son iguales
; continua y si son distintas devuelve falso
;supone que las cadenas son del mismo tamaño
(define (haymorfismo? s1 s2 añadidos diccionario)
  (if (equal? s1 "") #t
      (if (esta? (string-cabeza s1) añadidos)
      (if (equal? (hash-ref diccionario (string-cabeza s1)) (string-cabeza s2))
          (haymorfismo? (string-cola s1) (string-cola s2) añadidos diccionario)
          #f)
      (haymorfismo? (string-cola s1) (string-cola s2) (cons (string-cabeza s1) añadidos)
            (agregaydevuelve (string-cabeza s1) (string-cabeza s2) diccionario) )
      )
  )
)

;Funcion que devuelve true si un elemento esta en una lista, false si no esta
(define (esta? elemento lista)
  (if (empty? lista) #f
      (if (equal? (car lista) elemento) #t
      (esta? elemento (cdr lista))
      )
  )
)
;Funcion que agrega elementos a un diccionario y devuelve el diccionario
(define (agregaydevuelve s1 s2 dic)
  (hash-set! dic s1 s2)
  dic
)
;Funcion que da la cabeza de un string
(define (string-cabeza s)
  (substring s 0 1)
)
;Funcion que da la cola de un string
(define (string-cola s)
  (substring s 1 (string-length s))
)

; ------------------ Ejercicio 4 -------------------------
(define(automorfico? n)
    (define square (* n n))
    (if (= (remainder square 10) n)
        #true
        #false)
)

; ------------------ Ejercicio 5 -------------------------
;; Función auxiliar, asigna la cadena correspondiente a un número
;; postivo, negativo o cero.
(define(pos-neg-cero n)
  (cond
    [(= n 0) "Cero"]
    [(> n 0) "Positivo"]
    [(< n 0) "Negativo"])
)

(define(clasificar-num lst)
    (cond
      [(empty? lst) '()]
      [else (cons (pos-neg-cero (car lst)) (clasificar-num (cdr lst)))]
    )
)

; ------------------ Ejercicio 6 -------------------------
(define(simulador-dado n)
  (define (simular-aux lst suma-lst)
    (if (>= suma-lst n)
        lst
        (let ((lanzamiento (+ (random 6) 1)))
          (simular-aux (cons lanzamiento lst) (+ suma-lst lanzamiento)))))

  (simular-aux '() 0))
 ;; Aquí llamamos a la func con esos param para no modificar el resultado
 ;; q mata la recursión de simular-aux, lo llamamos para que nos devuelva la
 ;; lista final.

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
(define (generate-brackets n)
  (aux n n ""))

(define (aux left right current)
  (if (and (= left 0) (= right 0))
      (list current)
      (append
       (if (> left 0)
           (aux (- left 1) right (string-append current "("))
           '())
       (if (> right left)
           (aux left (- right 1) (string-append current ")"))
           '())
      )
  )
)

; ----------------- Ejercicio 10 ---------------------------
; count-steps-to-one: Una función recursiva recibe un n ́umero entero n y devuelve el n ́umero de pasos necesarios para reducir el n ́umero dado a 1 siguiendo estas reglas:
; Si es par, div ́ıdelo en 2.
; Si es impar, multipl ́ıcalo por 3 y s ́umale 1.
(define (count-steps-to-one n)
    (if (equal? n 1) 0
        (+ 1 (count-steps-to-one (if (even? n) (/ n 2) (+ (* n 3) 1))))))
