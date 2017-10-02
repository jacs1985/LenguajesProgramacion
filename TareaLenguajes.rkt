#lang racket
;;Primera Parte
;;Definicion de como se crea un arbol binario
#|Creamos dos constructores declarando asi como se encuentra compuesto
el nodo con su valor y sus hijos|#
;;Primer Constructor
(define (make-bt valorNodo hijoIzq hijoDer)
  (list valorNodo hijoIzq hijoDer))

#|Segundo Constructor: declarando vacio para cuando un nodo
tenga hijos sin valor, es decir nodos hojas|#
(define nodoVacio-bt '())

#|Procedemos a definir la estructura de seleccion del arbol binario|#
(define (valorNodo-bt arbol)
  (car arbol))
(define (hijoIzq-bt arbol)
  (car (cdr arbol)))
(define (hijoDer-bt arbol)
  (car (cdr (cdr arbol))))

;;Arbol vacio
(define (nodoVacio-bt? arbol)
  (null? arbol))

#|Se pregunta si el nodo es hoja, para ver si se declaran
sus hijos como vacios|#
(define (nodoHoja-bt arbol)
  (and (nodoVacio-bt? (hijoIzq-bt arbol))
       (nodoVacio-bt? (hijoDer-bt arbol))))

#|Comprobamos la creación de nuestro árbol y lo formamos
 a lista en inorden|#
(define (to-list-bt arbol)
	(if (nodoVacio-bt? arbol)
		'()
		(append (to-list-bt (hijoIzq-bt arbol))
			(list (valorNodo-bt arbol))
			(to-list-bt (hijoDer-bt arbol)))))
#|
(define nodo1 (make-bt 4 nodoVacio-bt nodoVacio-bt))
(define nodo2 (make-bt 3 nodoVacio-bt nodoVacio-bt))
(define nodo3 (make-bt '* nodo1 nodo2))
(define nodo4 (make-bt 6 nodoVacio-bt nodoVacio-bt))
(define nodo5 (make-bt '+ nodo4 nodo3))
(to-list-bt nodo5)
|#


;;Segunda Parte
;;Modificación de grafos (arbol)
;;Función Insertar un nodo
   #|Si es igual se devuelve el arbol binario tal cual.
     Si es menor llamar al constructor e insertar en el
       lado izquierdo y el derecho se devuelve tal cual.
     Si es mayor se hace la llamada recursiva por el lado
     derecho.
   |#

(define (insertarNodo-bt x arbol)
	(cond 
		((nodoVacio-bt? arbol) (make-bt x nodoVacio-bt nodoVacio-bt))
		((< x (valorNodo-bt arbol))
			(make-bt (valorNodo-bt arbol)
				(insertarNodo-bt x (hijoIzq-bt arbol))
				(hijoDer-bt arbol)))
		((> x (valorNodo-bt arbol))
			(make-bt (valorNodo-bt arbol)
				(hijoIzq-bt arbol)
				(insertarNodo-bt x (hijoDer-bt arbol))))
		((= x (valorNodo-bt arbol)) arbol)))


#|Implementación de la función insertar nodo
(define nodo1 (make-bt 7 nodoVacio-bt nodoVacio-bt))
(define nodo2 (make-bt 36 nodoVacio-bt nodoVacio-bt))
(define nodo3 (make-bt 30 nodo1 nodo2))
(define nodo4 (make-bt 45 nodoVacio-bt nodoVacio-bt))
(define nodo5 (make-bt 40 nodo3 nodo4))

(define nodo6  (insertarNodo-bt 38 nodo5))
(to-list-bt nodo6)
|#


#|Implementación de la función insertar con una lista 

|#
(define (insertarLista-bt lista arbol)
	(if (null? lista)
		arbol
		(insertarLista-bt (cdr lista)(insertarNodo-bt (car lista) arbol))))

#|Comprobamos implementación
(define nodo6 (insertarLista-bt '(38 5 50) nodo5))
(to-list-bt nodo6)
|#


;;Funcion eliminar Nodo
(define (eliminaNodo x arbol)
  
	(if (null? arbol)
		'()
	(if (eq? x (car arbol))
		(eliminaNodo x (cdr arbol))
		(cons (car arbol)(eliminaNodo x (cdr arbol))))))

