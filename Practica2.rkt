; INTEGRANTES:
;; Bravo Orozco Diego 320222904
;; Gutierrez Morales Joel Isaac 321259479
;; Reyes Colín Luis Manuel 321090474


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


; ------------------- Ejercicio 1 ------------------------
; Toma la lista, convierte a binario cada elemento, y pasa de little-endian a big-endian
(define (a-binario lst) (my-map reverse (my-map (letrec ([binario-de (lambda (n) (if (equal? n 0) empty (cons (modulo n 2) (binario-de (quotient n 2)))))]) binario-de) lst)))

; -------------------- Ejercicio 2 -----------------------
(define (filter-palindromos lst)
   (filter
    (lambda (n)
      (= n
       (let voltear ([num n] [rev 0])
         (if(<= num 0) rev
            (voltear (quotient num 10) (+ (* rev 10) (remainder num 10) ) )
         )
       )
       )
    )
   lst)
)

; ------------------ Ejercicio 3 -------------------------
; Define una estructura llamada juego que represente el
; estado del juego.
(struct juego (mazo en-juego manos turno) #:transparent)

; Usamos el tipo #:transparent para que Racket muestre/imprima
; el contenido de la struct de manera "legible"


; ------------------ Ejercicio 4 -------------------------
; Estructura carta.
(struct carta (color valor) #:transparent)
; Crea un mazo inicial con todas las cartas necesarias.
(define (generar-mazo)
  (append
   (apply append
          (for/list ([color '(rojo azul verde amarillo)])
            (append
             (list (carta color 0))
             (apply append
                    (for/list ([valor (range 1 10)])
                      (list (carta color valor) (carta color valor)))))))
   (apply append
          (for/list ([color '(rojo azul verde amarillo)])
            (apply append
                   (for/list ([valor '(skip reverse draw-2)])
                     (list (carta color valor) (carta color valor))))))
   (apply append
          (for/list ([valor '(wild draw-4)])
            (for/list ([i (range 4)])
              (carta 'especial valor))))))

; Constructor crear-juego

(define (crear-juego jugadores)
  (let* (
    ; mazo inicial ya revuelto.
    (mazo (shuffle (generar-mazo)))
    ; Reparte 7 cartas a cada jugador. Usamos map para aplicar la función
    ; (lambda sin parametros) de tomar 7 cartas del mazo a la lista de n
    ; jugadores, devolviendo así, la lista de la lista de cartas de cada
    ; jugador.
    (reparte (map (lambda(_) (take mazo 7)) (range jugadores)))
    ; Montón; cartas que sobran después de repartir.
    (nuevo-mazo (drop mazo (* 7 jugadores)))
    ; Selecciona la primera carta del mazo como en juego.
    (en-juego (first nuevo-mazo))
    ; Montón nuevo; cartas que sobran después de poner la primera como
    ; en juego.
    (nuevo-mazo (rest nuevo-mazo))

   )
  ; Configura el turno en 0 (primer jugador).
  (juego nuevo-mazo en-juego reparte 0)
  )
)

; ------------------ Ejercicio 5 -------------------------
(define (jugar-carta juego jugador carta)
  (let* ([carta-actual (juego-en-juego juego)]
         [mano (list-ref (juego-manos juego) jugador)])
    (if (or (equal? (carta-color carta) (carta-color carta-actual))
            (equal? (carta-valor carta) (carta-valor carta-actual))
            (equal? (carta-color carta) 'especial))
        (let* ([nueva-mano (remove carta mano)]
               [nuevas-manos (list-set (juego-manos juego) jugador nueva-mano)]
               [nuevo-turno (modulo (+ (juego-turno juego) 1) (length nuevas-manos))])
          (juego (juego-mazo juego) carta nuevas-manos nuevo-turno))
        (error "Carta invalida."))))
