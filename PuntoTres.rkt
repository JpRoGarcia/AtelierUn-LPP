#lang racket
; 3. Generar una nueva lista a partir de los datos de otra lista sin duplicados.
(define lista(list 1 3 2 6 4 5 9 8 7 4 2 3 2 1 6 9 8 7))

(printf "La Nueva Lista es: ")
(remove-duplicates lista)