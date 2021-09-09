#lang racket
; 1. Cálculo del área de un triangulo equilatero,
;    conociendo la longitud de uno de sus lados.

(define (are_triangulo l)(/(* (sqrt 3) (expt l 2) ) 4))

(printf "El Area de Triangulo es: ")
(are_triangulo 7.5)