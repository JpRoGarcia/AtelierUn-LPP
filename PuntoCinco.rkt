#lang racket
;definir el nodo
(define (nodo dato izq der)
  (list dato izq der)
  )
;retornar el dato
(define (raiz ab)
  (car ab))
;retornar el subarbol izquierdo
(define (izquierdo ab)
  (cadr ab))
;retornar el subarbol derecho
(define (derecho ab)
  (caddr ab))
;validar si el arbol esta vacio
(define (vacio-ab? ab)
  (null? ab))
;validar si un nodo es una hoja
(define (hoja-ab? ab)
  (and (vacio-ab? (derecho ab))
       (vacio-ab? (izquierdo ab))
  )
  )
;Asumiendo un BST(binary search tree), implementar función member.
;buscar dato, verdadero si lo encuentra y falso si no.

(define (buscar-ab? x bst)
  (cond
    [(vacio-ab? bst) #f]
    [(= x (raiz bst)) #t]
    [(< x (raiz bst)) (buscar-ab? x (izquierdo bst))]
    [(> x (raiz bst)) (buscar-ab? x (derecho bst))]
    )
  )

;agregar un dato al arbol binario
;crear un nuevo arbol a partir de uno existente

(define (agregar-dato-ab x bst)
  (cond
    [(vacio-ab? bst) (nodo x null null)]
    [(< x (raiz bst))
     (nodo
      (raiz bst)
      (agregar-dato-ab x (izquierdo bst))
      (derecho bst)
      )
     ]
    [(> x (raiz bst))
     (nodo
      (raiz bst)
      (izquierdo bst)
      (agregar-dato-ab x (derecho bst))
      )
     ]
    [else bst]
    )
)

;agregar una lista de datos

(define (agregar-lista-ab lista bst)
  (if (vacio-ab? lista)
      bst
      (agregar-lista-ab (cdr lista) (agregar-dato-ab (car lista) bst))
      )
  )

;realizar la sumatoria de elementos del arbol

;------------------------------------------------
; 4. Para un árbol binario, indicar cuantos nodos y hojas tiene.
(define (mostrar-nodos-ab ab)
  (if (vacio-ab? ab)
      '()
      (append
       (mostrar-nodos-ab (izquierdo ab))
       (list (raiz ab))
       (mostrar-nodos-ab (derecho ab))
       )
   )
  )

;Numero de Nodos
(define (NumeroNodos bst)
  (length (mostrar-nodos-ab bst)))

;Definimos el Grado
(define (Grado ab)
  (cond
    [(vacio-ab? ab) 0]
    [(and (equal? (Grado (derecho ab)) null)(equal? (Grado (derecho ab)) null)) 1]
    [(+ 1 (Grado (derecho ab)))]
    [(+ 1 (Grado (izquierdo ab)))]

  ))

;Nodos Necesarios
(define (TotalNodos grado)
  (- (expt 2 grado) 1))

;Verficar
(define (Comprobar NumeroNodos TotalNodos)
  (if (equal? NumeroNodos TotalNodos) "Balanciadito" "No Balanciadito"))

;------------------------------------------------

;arbol no Balanciadito
(define m7 (nodo 35 null null))
(define m6 (nodo 25 null null))
(define m5 (nodo 15 null null))
(define m4 (nodo 5 null null))
(define m3 (nodo 30 m6 m7))
(define m2 (nodo 10 m4 m5))
(define m1 (nodo 20 m2 m3))

;arbol Balanciadito
(define n8 (nodo 6 null null))
(define n7 (nodo 9 null null))
(define n6 (nodo 18 null null))
(define n5 (nodo 22 null null))
(define n4 (nodo 8 n8 n7))
(define n3 (nodo 20 n6 n5))
(define n2 (nodo 17 null n3))
(define n1 (nodo 12 n4 n2))

(printf "El Arbol Binario n1 esta: ")
(Comprobar (NumeroNodos n1) (TotalNodos (Grado n1)))

(printf "El Arbol Binario m1 esta: ")
(Comprobar (NumeroNodos m1) (TotalNodos (Grado m1)))
