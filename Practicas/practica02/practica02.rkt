#lang plai
;; any?: any-value --> boolean
;; Recibe cualquier tipo de valor y siempre regresa un true
(define (any? x) #t)

;; Ejercicio 1
;; Se define un tipo de dato Array que describe un arreglo con un
;; constructor de tipo MArray, donde sus parametros son msize el
;; tamaño del arreglo y marr la lista de elementos
(define-type Array
  [MArray (msize any?) (marr list?)])

;; test
;(test (MArray 0 '()) (MArray 0 '()))
;(test (MArray 1 '(7)) (MArray 1'(7)))
;(test (MArray 2 '(3 4)) (MArray 2 '(3 4)))
;(test (MArray 6 '(1 2 3)) (MArray 6 '(1 2 3)))
;(test (MArray 5 '(1 2 3 4 5)) (MArray 5 '(1 2 3 4 5)))

;=================================================================
;=================================================================

;; Ejercicio 2
;; Se define un tipo de dato recursivo MList que describe una lista,
;; teniendo un constructor de tipo MCons y una lista vacía MEmpty
(define-type MList
  [MCons (mhead any?) (mrest MList?)]
  [MEmpty])

;; test
;(test (MEmpty) (MEmpty)) 
;(test (MCons 2 (MEmpty)) (MCons 2 (MEmpty))) 
;(test (MCons 2 (MCons 9 (MEmpty))) (MCons 2 (MCons 9 (MEmpty))))
;(test (MCons 2 (MCons 9 (MCons 5 (MEmpty)))) (MCons 2 (MCons 9 (MCons 5 (MEmpty)))))
;(test (MCons 2 (MCons 9 (MCons 5 (MCons 8 (MEmpty))))) (MCons 2 (MCons 9 (MCons 5 (MCons 8 (MEmpty))))))

;=================================================================
;=================================================================

;; Ejercicio 3
;; Se define un tipo de dato recursivo NTree que describe un árbol n-ário
;; teniendo como constructor NTree y una hoja vacía TLEmpty
(define-type NTree
  [TLEmpty]
  [NodeN (elem any?) (sons (listof NTree?))])

;; test
;(test (TLEmpty) (TLEmpty))
;(test (NodeN 6 (list (TLEmpty))) (NodeN 6 (list (TLEmpty))))
;(test (NodeN 6 (list (TLEmpty) (TLEmpty) (TLEmpty))) (NodeN 6 (list (TLEmpty) (TLEmpty) (TLEmpty))))
;(test (NodeN 6 (list (TLEmpty) (NodeN 3 (list (TLEmpty))))) (NodeN 6 (list (TLEmpty) (NodeN 3 (list (TLEmpty))))))
;(test (NodeN 6 (list (NodeN 4 (list (TLEmpty))) (NodeN 3 (list (TLEmpty))) (NodeN 9 (list (TLEmpty)))))
;      (NodeN 6 (list (NodeN 4 (list (TLEmpty))) (NodeN 3 (list (TLEmpty))) (NodeN 9 (list (TLEmpty))))))

;=================================================================
;=================================================================

;; Ejercicio 4
;; Se define un tipo de dato Position que describe una posición en el plano
;; cartesiano, teniendo un constructor 2D-Point que toma dos números reales
(define-type Position
  [2D-Point (cX number?) (cY number?)])

;; test
;(test (2D-Point 0 0) (2D-Point 0 0))
;(test (2D-Point 1 2) (2D-Point 1 2))
;(test (2D-Point -20 10) (2D-Point -20 10))
;(test (2D-Point 4.34 0) (2D-Point 4.34 0))
;(test (2D-Point (sqrt 3) (+ 3 4.8)) (2D-Point 1.7320508075688772 7.8))

;=================================================================
;=================================================================

;; Ejercicio 5
;; Se define un tipo de dato Figure, que describe tres figuras geométricas,
;; teniendo tres constructores Circle, Square y Rectangle
(define-type Figure
  [Circle (center Position?) (radio number?)]
  [Square (corner Position?) (height number?)]
  [Rectangle (corner Position?) (width number?) (height number?)])

;(test (Circle (2D-Point 0 0) 1) (Circle (2D-Point 0 0) 1))
;(test (Circle (2D-Point 1 3) 10) (Circle (2D-Point 1 3) 10))
;(test (Square (2D-Point 0 0) 5) (Square (2D-Point 0 0) 5))
;(test (Square (2D-Point -3 -2) 10) (Square (2D-Point -3 -2) 10))
;(test (Rectangle (2D-Point 0 0) 10 5) (Rectangle (2D-Point 0 0) 10 5))

;=================================================================
;=================================================================
;; Ejercicio 6
;; setvalueA: Array number number --> Array
;; Dado un Array, una posición y un valor, regresa un nuevo Array
;; que difiere del original solo en la posicion con el valor dado
(define (setvalueA arr pos val)
  (type-case Array arr
    [MArray (msize marr) (cond
                           [(or (>= pos msize) (< pos 0)) "error"]
                           [else (MArray msize (aux-setvalueA marr pos val))])]))

;; aux-setvalueA: listof-values number number --> listof-values
;; Dada una lista de valores, una posición y un valor, regresa una lista
;; que difiere de la original solo en la posición con el valor dado
(define (aux-setvalueA ar pos val)
  (cond
    [(= pos 0) (cons val (cdr ar))]
    [else (cons (car ar) (aux-setvalueA (cdr ar) (sub1 pos) val))]))

;; test
;(define ar (MArray 5 '(0 0 0 0 0)))
;(test (setvalueA ar 0 9) (MArray 5 '(9 0 0 0 0)))
;(test (setvalueA ar 1 9) (MArray 5 '(0 9 0 0 0)))
;(test (setvalueA ar 4 9) (MArray 5 '(0 0 0 0 9)))
;(test (setvalueA ar -1 9)  "error")
;(test (setvalueA ar 5 9) "error")
;(test (setvalueA ar 8 9) "error")

;=================================================================
;=================================================================

;; Ejercicio 7
;; MArray2MList: Array --> MList
;; Dado un tipo de dato Array, regresa un tipo de dato MList que contiene
;; todos los elementos de Array
(define (MArray2MList arr)
  (type-case Array arr
      [MArray (msize marr) (cond
                             [(empty? marr) (MEmpty)]
                             [else (MCons (car marr) (MArray2MList (MArray (sub1 msize) (cdr marr))))]
                             )]))
;; define 
;(define arr1 (MArray 0 '()))
;(define arr2 (MArray 1 '(1)))
;(define arr3 (MArray 2 '(1 2)))
;(define arr4 (MArray 3 '(1 2 3)))
;(define arr5 (MArray 4 '("a" "b" "c" "d")))
;; test
;(test (MArray2MList arr1) (MEmpty))
;(test (MArray2MList arr2) (MCons 1 (MEmpty)))
;(test (MArray2MList arr3) (MCons 1 (MCons 2 (MEmpty))))
;(test (MArray2MList arr4) (MCons 1 (MCons 2 (MCons 3 (MEmpty)))))
;(test (MArray2MList arr5) (MCons "a" (MCons "b" (MCons "c" (MCons "d" (MEmpty))))))

;=================================================================
;=================================================================

;; Ejercicio 8
;; printML: MList --> string
;; Dado un tipo de dato MList, regresa una representación de la lista en un formato legible
(define (printML l)
  (~a "[" (aux-printML l) "]"))

(define (aux-printML l)
  (type-case MList l
    [MEmpty () ""]
    [MCons (mhead mrest) (cond
                           [(and (MCons? mhead) (MCons? mrest)) (~a "[" (aux-printML mhead) "], "  (aux-printML mrest))]
                           [(and (MCons? mhead) (MEmpty? mrest)) (~a "[" (aux-printML mhead) "]")]
                           [else (if (MEmpty? mrest)
                                     (~a mhead "")
                                     (~a mhead ", " (aux-printML mrest)))])]))

    
;; define
(define l1 (MEmpty))
(define l2 (MCons 2 (MEmpty)))
(define l3 (MCons 1 (MCons 3 (MEmpty))))
(define l4 (MCons 8 (MCons 3 (MCons 1 (MEmpty)))))
(define l5 (MCons (MCons 1 (MCons 2 (MEmpty))) (MCons 3 (MEmpty))))
(define l6 (MCons (MCons 1 (MCons 2 (MEmpty))) (MCons (MCons 3 (MCons 4 (MEmpty))) (MEmpty))))

;; test
(test (printML l1) "[]")
(test (printML l2) "[2]")
(test (printML l3) "[1, 3]")
(test (printML l5) "[[1, 2], 3]")
(test (printML l6) "[[1, 2], [3, 4]]")

;=================================================================
;=================================================================

;; Ejercicio 9
;; concatML: MList MList --> MList
;; Dadas dos listas de tipo MList, regresa una MList que es
;; la concatenación de las dos listas originales
(define (concatML l1 l2)
  (type-case MList l1
    [MEmpty () l2]
    [MCons (mhead mrest) (MCons mhead (concatML mrest l2))]))

;; define
;(define l1 (MEmpty))
;(define l2 (MCons 2 (MEmpty)))
;(define l3 (MCons 3 (MEmpty)))
;(define l4 (MCons 8 (MCons 3 (MCons 1 (MEmpty)))))
;; test
;(test (concatML l1 l1) (MEmpty))
;(test (concatML l1 l2) (MCons 2 (MEmpty)))
;(test (concatML l2 l1) (MCons 2 (MEmpty)))
;(test (concatML l2 l4) (MCons 2 (MCons 8 (MCons 3 (MCons 1 (MEmpty))))))
;(test (concatML l4 l2) (MCons 8 (MCons 3 (MCons 1 (MCons 2 (MEmpty))))))

;=================================================================
;=================================================================

;; Ejercicio 10
;; lengthML: MList --> number
;; Dada un tipo de dato MList regresa la longitus de la lista
(define (lengthML l)
  (type-case MList l
    [MEmpty () 0]
    [MCons (mhead mrest) (+ 1 (lengthML mrest))]))

;; define
;(define list1 (MEmpty))
;(define list2 (MCons 2 (MEmpty)))
;(define list3 (MCons 3 (MCons 6 (MEmpty))))
;(define list4 (MCons 8 (MCons 3 (MCons 1 (MEmpty)))))
;;test
;(test (lengthML list1) 0)
;(test (lengthML list2) 1)
;(test (lengthML list3) 2)
;(test (lengthML list4) 3)
;(test (lengthML (concatML list3 list4)) 5)

;=================================================================
;=================================================================

;; Ejercicio 11
;; mapML: MList function --> MList
;; Dada un tipo de dato MList y una funcion de aridad 1, regresa
;; la aplicación de la función a cada uno de los elementos de la
;; lista original en una MList
(define (mapML l fun)
  (type-case MList l
    [MEmpty () (MEmpty)]
    [MCons (mhead mrest) (MCons (fun mhead)(mapML mrest fun))]))

;; define
;(define list1 (MEmpty))
;(define list2 (MCons 2 (MEmpty)))
;(define list3 (MCons 3 (MCons 6 (MEmpty))))
;(define list4 (MCons 8 (MCons 3 (MCons 1 (MEmpty)))))
;; test
;(test (mapML list1 add1) (MEmpty))
;(test (mapML list2 add1) (MCons 3 (MEmpty)))
;(test (mapML list4 add1) (MCons 9 (MCons 4 (MCons 2 (MEmpty)))))
;(test (mapML list3 (lambda (n) (* 2 n))) (MCons 6 (MCons 12 (MEmpty))))
;(test (mapML list3 symbol?) (MCons #f (MCons #f (MEmpty))))

;=================================================================
;=================================================================

;; Ejercicio 12
;; filterML: MList predicate --> MList
;; Dada un tipo de dato MList y un predicado  de un argumento, regresa
;; una MList con los elemetos de la lista original que cumplan el predicado
(define (filterML l pred)
  (type-case MList l
    [MEmpty () (MEmpty)]
    [MCons (mhead mrest) (if (pred mhead)
                             (MCons mhead (filterML mrest pred))
                             (filterML mrest pred))]))
;; define
;(define list1 (MEmpty))
;(define list2 (MCons 10 (MEmpty)))
;(define list3 (MCons 0 (MCons 6 (MEmpty))))
;(define list4 (MCons 8 (MCons 3 (MCons 1 (MEmpty)))))
;(define list5 (MCons 1 (MCons "a" (MCons 2 (MCons "b" (MCons 3 (MEmpty)))))))
;; test
;(test (filterML list1 (lambda (x) (not (zero? x)))) (MEmpty))
;(test (filterML list2 (lambda (x) (not (zero? x)))) (MCons 10 (MEmpty)))
;(test (filterML list3 (lambda (x) (not (zero? x)))) (MCons 6 (MEmpty)))
;(test (filterML list4 (lambda (x) (zero? x))) (MEmpty))
;(test (filterML list5 (lambda (x) (string? x))) (MCons "a" (MCons "b" (MEmpty))))

;=================================================================
;=================================================================

;; Ejercicio 17
;; area: Figure --> number
;; Dada una tipo de dato Figure regresa el área de la figura
(define (area fig)
  (type-case Figure fig
    [Circle (center radio) (* pi (* radio radio))]
    [Square (corner height) (* height height)]
    [Rectangle (corner width height) (* width height)]))

;; define
;(define c1 (Circle (2D-Point 0 0) 1))
;(define c2 (Circle (2D-Point 3 3) 3))
;(define s1 (Square (2D-Point 0 0) 5))
;(define s2 (Square (2D-Point 2 5) 10))
;(define r1 (Rectangle (2D-Point 0 0) 6 3))
;(define r2 (Rectangle (2D-Point 1 1) 20 40))

;(test (area c1) pi)
;(test (area c2) 28.274333882308138)
;(test (area s1) 25)
;(test (area s2) 100)
;(test (area r1) 18)
;(test (area r2) 800)

;=================================================================
;=================================================================

;; Ejercicio 18
;; in-figure?: Figure Position --> boolean
;; Dada una tipo de dato Figure y un tipo de dato Position regresa un true
;; si el punto esta dentro de la figura y false en caso de que no
(define (in-figure? fig point)
  (type-case Figure fig
    [Circle (center radio) (type-case Position point
                               [2D-Point (cX cY) (if (<= (sqrt (+ (sqr(- cX (getX center))) (sqr(- cY (getY center))))) radio)
                                                     #t
                                                     #f)])]
    [Square (corner height) (if (and (>= (getX point) (getX corner)) (<= (getX point) (+ height (getX corner)))
                                     (>= (getY point) (getY corner)) (<= (getY point) (+ height (getY corner))))
                                #t
                                #f)]
    [Rectangle (corner width height) (if (and (>= (getX point) (getX corner)) (<= (getX point) (+ width (getX corner)))
                                              (>= (getY point) (getY corner)) (<= (getY point) (+ height (getY corner))))
                                         #t
                                         #f)]))

;; getX: Position --> number
;; Dada un dato de tipo Position, regresa la coordenada X
(define (getX pos)
  (type-case Position pos
    [2D-Point (cX cY) cX]))

;; getY: Position --> number
;; Dada un dato de tipo Position, regresa la coordenada Y
(define (getY pos)
  (type-case Position pos
    [2D-Point (cX cY) cY]))
                           
;(test (in-figure? (Circle (2D-Point 0 0) 4) (2D-Point 3 0)) #t)
;(test (in-figure? (Circle (2D-Point 0 0) 4) (2D-Point 4 4)) #f)
;(test (in-figure? (Square (2D-Point 5 5) 4) (2D-Point 6 6)) #t)
;(test (in-figure? (Square (2D-Point 5 5) 4) (2D-Point 2 2)) #f)
;(test (in-figure? (Rectangle (2D-Point 5 5) 4 6) (2D-Point 4 4)) #f)
;(test (in-figure? (Rectangle (2D-Point 5 5) 4 6) (2D-Point 7 8)) #t)


      

    