#lang plai

;; Ejercicio 1
;; mpow: number number --> number 
;; Eleva un número a una cierta potencia (segundo argumento)
(define (mpow a n)
  (cond 
    [(and (zero? a) (zero? n)) "valor indefinido"]
    [(zero? n) 1]
    [else (* a (mpow a (- n 1)))]))

(test (mpow 10 0) 1)
(test (mpow 3 2) 9)
(test (mpow 1 20) 1)
(test (mpow -9 2) 81)
(test (mpow 2 8) 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 2
;; maverage: list-of numbers --> number 
;; Toma una lista de números y obtiene el promedio de esta lista
(define (maverage l)
  (cond
    [(empty? l) 0]
    [else (/ (addElems l) (mlong l))]))

;; mlong: listof-values --> number 
;; Toma una lista de elementos y regresa la cantidad de elementos
(define (mlong l)
  (cond 
    [(empty? l) 0]
    [else (+ 1 (mlong (cdr l)))]))

;; addElems: listof-numbers --> number 
;; Toma una lista de números y regresa la suma de 
;; todos los elementos
(define (addElems l)
  (cond 
    [(empty? l) 0]
    [else (+ (car l) (addElems (cdr l)))]))

(test (maverage '(1 2 3 4 5)) 3)
(test (maverage '(4)) 4)
(test (maverage '(5 5 5 5 5 5 5)) 5)
(test (maverage '(10 -10)) 0)
(test (maverage '(32 4 3)) 13)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 3
;; mprimes: number --> listof-numbers 
;; Dado un número entero positivo regresa una lista de números
;; primos entre el 2 y el número dado
(define (mprimes n) 
  (cond 
    [(= n 1) '()]
    [(if (isprime? n)
         (append (mprimes (sub1 n)) (list n))
         (mprimes (sub1 n)))]))

;; isprime?: number --> boolean 
;; Dado un número entero positivo mayor o igual a 2 determina
;; si es un número primo
(define (isprime? n)
  (isprime2? 2 n))

;; isprime2? number number --> boolean
;; Dado un número entero determina si entre un número k (menor a n)
;; y n existe un número que divida a n
(define (isprime2? k n) 
  (cond
    [(<= n 1) #f]
    [(= k n) #t]
    [else (if (= (modulo n k) 0)
         #f
         (isprime2? (add1 k) n))]))

(test (mprimes 1) '())
(test (mprimes 2) '(2))
(test (mprimes 10) '(2 3 5 7))
(test (mprimes 8) '(2 3 5 7))
(test (mprimes 33) '(2 3 5 7 11 13 17 19 23 29 31))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 4
;; mzip: listof-values listof-values --> listof-values 
;; Dadas dos listas de elementos empareja uno a uno los elementos
;; de una lista con la otra lista, regresando una lista de parejas
(define (mzip l1 l2)
  (cond 
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (list (car l1) (car l2)) (mzip (cdr l1) (cdr l2)))]))

(test (mzip '(1 2 3) '()) '())
(test (mzip '() '(1 2 3)) '())
(test (mzip '(1 2 3 4) '(1 2 3)) '((1 1) (2 2) (3 3)))
(test (mzip '(1 2 3 4) '(1 2 3 4 5 6 7 8 9)) '((1 1) (2 2) (3 3) (4 4)))
(test (mzip '(1 2 3 4 5 6 7 8 9) '(1 2 3 4 5)) '((1 1) (2 2) (3 3) (4 4) (5 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 5
;; mreduce: function listof-values --> listof-values 
;; Dada una función de aridad dos  y una lista de elementos, devuelve 
;; la evaluación de la función a los elementos de forma ligada
(define (mreduce func l)
  (cond 
    [(< (mlong l) 2) empty]
    [(if (empty? (cddr l))
        (func (car l) (cadr l))
        (func (car l) (mreduce func (cdr l))))]))

(test (mreduce + '(5 1 1 1 1 1)) 10)
(test (mreduce - '(5 1 1 1 1 1)) 4)
(test (mreduce mzip '((a b c) (a b c) (a b c))) '((a (a a)) (b (b b)) (c (c c))))
(test (mreduce * '(1 2 3 4 5)) 120)
(test (mreduce append '((1) (2) (3 4))) '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 6
;; mconcat: listof-values listof-values --> listof-values
;; Toma dos listas de elementos y regresa la concatenación
;; de la primer lista con la segunda
(define (mconcat l1 l2)
  (cond 
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (cons (car l1) (mconcat (cdr l1) l2))]))

(test (mconcat '() '()) '())
(test (mconcat '(1 2 3 4) '()) '(1 2 3 4))
(test (mconcat '() '(1 2 3 4)) '(1 2 3 4))
(test (mconcat '(1 2 3) '(3 4)) '(1 2 3 3 4))
(test (mconcat '(1 2 3 4) '(1 2 3 4 5)) '(1 2 3 4 1 2 3 4 5))
(test (mconcat '(a b) '(a b)) '(a b a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 7
;; mmap: function listof-values --> listof-values
;; Dada una función de un argumento  y una lista de elementos,
;; regresa una lista con la aplicación a cada elemento de la 
;; lista original
(define (mmap func l)
  (cond 
    [(empty? l) '()]
    [else (cons (func (car l)) (mmap func (cdr l)))]))

(test (mmap sub1 '(1 1 1 1)) '(0 0 0 0))
(test (mmap mprimes '(1 2 7 10)) '(() (2) (2 3 5 7) (2 3 5 7)))
(test (mmap maverage '((1) (2 2) (3 3 3))) '(1 2 3))
(test (mmap zero? '(1 1 1 0)) '(#f #f #f #t))
(test (mmap cadr '((1 a 2) (1 b 2) (1 c 3))) '(a b c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 8
;; mfilter: predicate listof-values --> listof-values
;; Dado un predicado de un argumento y una lista de elementos 
;; regresa una lista con los elementos que cumplen el predicado
(define (mfilter pred l)
  (cond 
    [(empty? l) '()]
    [(if (pred (car l))
         (cons (car l) (mfilter pred (cdr l)))
         (mfilter pred (cdr l)))]))

(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))
(test (mfilter (lambda (x) (symbol? x)) '(a b 1 2 r 5)) '(a b r))
(test (mfilter (lambda (n) (< n 10)) '(10 11 12 3 5 20)) '(3 5))
(test (mfilter (lambda (y) (isprime? y)) '(1 2 5 20 10 4 23 6 7)) '(2 5 23 7))
(test (mfilter (lambda (l) (>= (mlong l) 5)) '((1 2) () (1 2 3 4 5) (2))) '((1 2 3 4 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 9
;; many?: predicate listof-values --> boolean
;; Dado un predicado de un argumento  y una lista de elementos 
;; regresa #t si alguno de los elementos de la lista cumple el predicado
(define (many? pred l)
  (cond 
    [(empty? l) #f]
    [else (or (pred (car l)) (many? pred (cdr l)))]))

(test (many? zero? '(0 1 2 3 4 5)) #t)
(test (many? symbol? '(0 1 2 3 4 5)) #f)
(test (many? isprime? '(0 1 2 3 4 6)) #t)
(test (many? symbol? '()) #f)
(test (many? empty? '((1 2) () (2 4 5))) #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 10
;; mevery?: predicate listof-values --> boolean
;; Dado un predicado de un argumento y una lista de elementos regresa
;; #t si y solo si todos los elementos de la lista cumplen con el predicado
(define (mevery? pred l)
  (cond 
    [(empty? l) #t]
    [else (and (pred (car l)) (mevery? pred (cdr l)))]))

(test (mevery? zero? '()) #t)
(test (mevery? zero? '(0 1 2 3 4 5)) #f)
(test (mevery? isprime? '(0 1 2 3 4 5)) #f)
(test (mevery? isprime? '(23 7 2 3 31)) #t)
(test (mevery? symbol? '(a b c 4 e d f)) #f)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ejercicio 11
;; mpowerset: listof-values --> listof-values
;; Toma una lista de elementos y regresa su conjunto potencia
(define (mpowerset l)
  (cond 
    [(empty? l) (list '())]
    [else (append (list '()) (mpowerset-aux 1 l))]))

;; mpowerset-aux: number listof-values --> listof-values
;; Dada una lista de elementos y un número entero positivo regresa 
;; una lista de subconjuntos a partir de una cierta longitud determinada 
;; por el número dado como argumento
(define (mpowerset-aux long-set l)
  (cond     
    [(= long-set (mlong l)) (msubsets long-set l)]
    [(< long-set (mlong l)) (append (msubsets long-set l) (mpowerset-aux (add1 long-set) l))]))

;; msubsets: number listof-values --> listof-values
;; Dado un número entero positivo y una lista de elementos regresa una 
;; lista con todos los posibles subconjuntos de una longitud determinada
;; por el número dado
(define (msubsets long-set l)
  (cond 
    [(= long-set 1) (genera 1 l)]
    [(= (mlong l) long-set) (genera long-set l)]
    [(> (mlong l) long-set) (append (genera long-set l) (msubsets long-set (cdr l)))]))

;; genera: number listof-values --> listof-values
;; Dado un número entero positivo y una lista de elementos regresa
;; una lista de subconjuntos con todas las posibles combinaciones con 
;; la cabeza de la lista tal que la longitud estadeterminada por el 
;; número dado
(define (genera l-subset l)
  (cond
    [(< (mlong l) l-subset) '()]
    [(= l-subset 1) (cons (list (car l)) (genera l-subset (cdr l)))]
    [else (cons (tupla l-subset l) (genera l-subset (cons (car l) (cddr l))))]))

;; tupla: number listof-values --> listof-values
;; Dado un número y una lista de elementos regresa una lista con los 
;; primeros n elementos , donde n esta determinada por el número dado
(define (tupla l-subset l)
  (cond 
    [(<= l-subset 0) '()]
    [else (cons (car l) (tupla (sub1 l-subset) (cdr l)))]))

(test (mpowerset '()) '(()))
(test (mpowerset '(a)) '(() (a)))
(test (mpowerset '(1 2)) '(() (1) (2) (1 2)))
(test (mpowerset '(1 2 3)) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)))
(test (mpowerset '(1 2 3 4)) '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 3 4) (2 3 4) (1 2 3 4)))
