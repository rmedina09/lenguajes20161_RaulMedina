#lang plai

(require "practica3-base.rkt")

; ========================================================================================
; Seccion I. Heart Rate Zones
; ========================================================================================

;; Ejercicio 1
;; zone: number number --> list-of-HRZ
;; Dado el ritmo cardiaco de descanso r y el maximo ritmo cardiaco máximo 
(define (zone r m)
  (list (resting (zone-min r m 0) (zone-max r m 0))
        (warm-up (zone-min r m 1) (zone-max r m 1))
        (fat-burning (zone-min r m 2) (zone-max r m 2))
        (aerobic (zone-min r m 3) (zone-max r m 3))
        (anaerobic (zone-min r m 4) (zone-max r m 4))
        (maximum (zone-min r m 5) (zone-max r m 5))))

;; zone-min: number number number --> HRZ
;; Dados el ritmo cardiaco de descanso y el máximo regresa el valor
;; minimo de la zona cardica correspondiente al índice que se le pase
(define (zone-min rest1 max1 i)
  (let ([range1 (- max1 rest1)])
    (cond
      [(= i 0) rest1]
      [else (+ rest1 (* range1 (+ 0.5 (* 0.1 (sub1 i)))))])))

;; zone-max: number number number --> HRZ
;; Dados el ritmo cardiaco de descanso y el máximo regresa el valor
;; maximo de la zona cardica correspondiente al índice que se le pase
(define (zone-max rest1 max1 i)
  (let ([range1 (- max1 rest1)])
    (cond
      [(= i 0) (+ rest1 (* range1 0.5) -1)]
      [(= i 5) (+ rest1 (* range1 (+ 0.5 (* 0.1 i))))]
      [else (+ rest1 (* range1 (+ 0.5 (* 0.1 i))) -1)])))

;(test (zone 50 180) (list (resting 50 114.0) (warm-up 115.0 127.0) (fat-burning 128.0 140.0) (aerobic 141.0 153.0) (anaerobic 154.0 166.0) (maximum 167.0 180.0)))
;(test (zone 60 120) (list (resting 60 89.0) (warm-up 90.0 95.0) (fat-burning 96.0 101.0) (aerobic 102.0 107.0) (anaerobic 108.0 113.0) (maximum 114.0 120.0)))
;(test (zone 50 160) (list (resting 50 104.0) (warm-up 105.0 115.0) (fat-burning 116.0 126.0) (aerobic 127.0 137.0) (anaerobic 138.0 148.0) (maximum 149.0 160.0)))
;(test (zone 70 180) (list (resting 70 124.0) (warm-up 125.0 135.0) (fat-burning 136.0 146.0) (aerobic 147.0 157.0) (anaerobic 158.0 168.0) (maximum 169.0 180.0)))
;(test (zone 60 200) (list (resting 60 129.0) (warm-up 130.0 143.0) (fat-burning 144.0 157.0) (aerobic 158.0 171.0) (anaerobic 172.0 185.0) (maximum 186.0 200.0)))

; ========================================================================================
; ========================================================================================

;; Ejercicio 2
;; get-zone: symbol list-of-HRZ --> HRZ
;; Dado un símbolo  que es el nombre de una zona y una lista de zones
;; regresar el tipo de dato correspondiente
(define (get-zone name l-zones)
  (cond
    [(symbol=? name 'resting) (car l-zones)]
    [(symbol=? name 'warm-up) (cadr l-zones)]
    [(symbol=? name 'fat-burning) (caddr l-zones)]
    [(symbol=? name 'aerobic) (cadddr l-zones)]
    [(symbol=? name 'anaerobic) (cadddr (cdr l-zones))]
    [(symbol=? name 'maximum) (cadddr (cddr l-zones))]
    [else (error 'get-zone "No existe")]))
    
  
(define my-zone (zone 50 180))

;; test
;(test (get-zone 'resting my-zone) (resting 50 114.0))
;(test (get-zone 'warm-up my-zone) (warm-up 115.0 127.0))
;(test(get-zone 'fat-burning my-zone) (fat-burning 128.0 140.0))
;(test(get-zone 'aerobic my-zone) (aerobic 141.0 153.0))
;(test (get-zone 'anaerobic my-zone) (anaerobic 154.0 166.0))
;(test (get-zone 'maximum my-zone) (maximum 167.0 180.0))

; ========================================================================================
; ========================================================================================

;; Ejercicio 3
;; bpm->zone: list-of-number list-of-HRZ --> list-of-HRZ
;; Dada una lista de frecuencias cardiacas y una lista de HRZ, regresa
;; una lista de HZR por cada frecuencia cardiaca
(define (bmp->zone lfc l-HRZ)
  (cond
    [(empty? lfc) empty]
    [else (let* ([fc (car lfc)]
                [r (if (and (>= fc (resting-low (car l-HRZ))) (<= fc (resting-high (car l-HRZ)))) (list (car l-HRZ)) empty)]
                [w (if (and (>= fc (warm-up-low (cadr l-HRZ))) (<= fc (warm-up-high (cadr l-HRZ)))) (list (cadr l-HRZ)) empty)]
                [f (if (and (>= fc (fat-burning-low (caddr l-HRZ))) (<= fc (fat-burning-high (caddr l-HRZ)))) (list (caddr l-HRZ)) empty)]
                [ae (if (and (>= fc (aerobic-low (cadddr l-HRZ))) (<= fc (aerobic-high (cadddr l-HRZ)))) (list (cadddr l-HRZ)) empty)]
                [an (if (and (>= fc (anaerobic-low (cadddr (cdr l-HRZ)))) (<= fc (anaerobic-high (cadddr (cdr l-HRZ))))) (list (cadddr (cdr l-HRZ))) empty)]
                [m (if (and (>= fc (maximum-low (cadddr (cddr l-HRZ)))) (<= fc (maximum-high (cadddr (cddr l-HRZ))))) (list (cadddr (cddr l-HRZ))) empty)])
            (append r w f ae an m (bmp->zone (cdr lfc) l-HRZ)))]))
      
;; test
;(test (bmp->zone empty my-zone) '())
;(test (bmp->zone '(50 60) my-zone) (list (resting 50 114.0) (resting 50 114.0)))
;(test (bmp->zone '(140 141) my-zone) (list (fat-burning 128.0 140.0) (aerobic 141.0 153.0)))
;(test (bmp->zone '(100 114 115) my-zone) (list (resting 50 114.0) (resting 50 114.0) (warm-up 115.0 127.0)))
;(test (bmp->zone '(50 115 128 141 154 180) my-zone) (list (resting 50 114.0) (warm-up 115.0 127.0) (fat-burning 128.0 140.0) (aerobic 141.0 153.0) (anaerobic 154.0 166.0) (maximum 167.0 180.0)))

; ========================================================================================
; ========================================================================================

;; Ejercicio 4
;; create-trackpoints: list-of-tuple-4 list-of-HRZ --> list-of-Frame
;; Dada una lista donde cada elemento es una tupla de 4 y una lista de HRZ, regresa
;; una lista de tranckpoints que contenga la información dada
(define (create-trackpoints l l-HRZ)
  (cond
    [(empty? l) empty]
    [else (let* ([gpslat (caadar l)]
                 [gpslong (car (cdadar l))]
                 [r (caddar l)]
                 [z (car (bmp->zone (cddar l) l-HRZ))]
                 [t (caar l)])
            (append (list (trackpoint (GPS gpslat gpslong) r z t)) (create-trackpoints (cdr l) l-HRZ)))]))

;; define
(define rd1 (take raw-data 1))
(define rd2 (take raw-data 2))
(define rd3 (take raw-data 3))
(define rd4 (take raw-data 4))

;; test
;(test (create-trackpoints empty my-zone) empty)
;(test (create-trackpoints rd1 my-zone) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)))
;(test (create-trackpoints rd2 my-zone) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654) (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655)))
;(test (create-trackpoints rd3 my-zone) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654) (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655) (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)))      
;(test (create-trackpoints rd4 my-zone) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654) (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655) (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658) (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662))) 


; ========================================================================================
; ========================================================================================

;; Ejercicio 5
;; total-distance: list-of-Frame --> number
;; Dada una lista de Frame (trackpoints), regresa la distancia total recorrida
(define (total-distance ltp)
  (cond
    [(empty? ltp) 0]
    [(empty? (cdr ltp)) 0]
    [else (let* ([gps1 (trackpoint-loc (car ltp))]
                 [gps2 (trackpoint-loc (cadr ltp))]
                 [h (haversine gps1 gps2)])
            (+ h (total-distance (cdr ltp))))]))

;; haversine: Coordinates Coordinates --> number
;; Dados dos valores GPS calcula la distancia entre ellos usando la formula de haversine
(define (haversine g1 g2)
  (let* ([d2r (/ pi 180)]
         [DLat (* (/ (- (GPS-lat g2) (GPS-lat g1)) 2) d2r)]
         [DLon (* (/ (- (GPS-long g2) (GPS-long g1)) 2) d2r)]
         [sen2DLat (* (sin DLat) (sin DLat))]
         [sen2DLon (* (sin DLon) (sin DLon))]
         [multCosSen2DLon (* (cos (GPS-lat g1)) (cos (GPS-lat g2)) sen2DLon)]                           
         [h (+ sen2DLat multCosSen2DLon)]
         [2R (* 2 6371)]
         [arcSenh (asin (sqrt h))])
    (* 2R arcSenh)))

;;define
(define tp1 (create-trackpoints rd1 my-zone))
(define tp2 (create-trackpoints rd2 my-zone))
(define tp3 (create-trackpoints (take raw-data 3) my-zone))
(define tp4 (create-trackpoints (take raw-data 4) my-zone))
(define tp5 (create-trackpoints (take raw-data 5) my-zone))
(define tp10 (create-trackpoints (take raw-data 10) my-zone))
(define tp100 (create-trackpoints (take raw-data 100) my-zone))
(define tps (create-trackpoints raw-data my-zone))

;; test
;(test (total-distance tp1) 0)
;(test (total-distance tp2) 0.0)
;(test (total-distance tp10) 0.048691066531181534)
;(test (total-distance tp100) 0.8701430216465783)
;(test (total-distance tps) 4.638691152686352)

; ========================================================================================
; ========================================================================================

;; Ejercicio 6
;; average-hr: list-of-Frame --> number
;; Dada una lista de tipo Frame (trackpoints), regresa el promedio del ritmo cardiaco
(define (average-hr lt)
  (cond
    [(empty? lt) 0]
    [else (exact-round (/ (addLF lt) (longLF lt)))]))

;; addLF: list-of-Frame --> number
;; Dada una lista de trackpoints, regresa la suma de los ritmos cardiacos de la lista
(define (addLF l)
  (cond
    [(empty? l) 0]
    [else (+ (trackpoint-hr (car l)) (addLF (cdr l)))]))

;; longLF: list-of-Frame --> number
;; Dada una lista de trackpoints, regresa el número de elementos de la lista
(define (longLF l)
  (cond
    [(empty? l) 0]
    [else (+ 1 (longLF (cdr l)))]))

;; test
;(test (average-hr empty) 0)
;(test (average-hr tp1) 104)
;(test (average-hr tp5) 107)
;(test (average-hr tp10) 111)
;(test (average-hr tp100) 134)
;(test (average-hr tps) 150)

; ========================================================================================
; ========================================================================================

;; Ejercicio 7
;; max-hr: list-of-Frame --> number
;; Dada una lista de tipo Frame (trackpoints), regresa el máximo ritmo cardiaco de la lista
(define (max-hr lf)
  (aux-max lf 0))

;; aux-max: list-of-Frame number --> number
;; Dada una lista de tipo Frame y un número, regresa el valor máximo entre los ritmos
;; cardiacosde la lista y el número
(define (aux-max l m)
  (cond
    [(empty? l) m]
    [else (if (< m (trackpoint-hr (car l))) (aux-max (cdr l) (trackpoint-hr (car l))) (aux-max (cdr l) m))]))

;; test
;(test (max-hr empty) 0)
;(test (max-hr tp1) 104)
;(test (max-hr tp2) 104)
;(test (max-hr tp5) 111)
;(test (max-hr tp10) 120)
;(test (max-hr tp100) 147)
;(test (max-hr tps) 165)

; ========================================================================================
; ========================================================================================

;; Ejercicio 8
;; collapse-trackpoints: list-of-Frame number --> list-of-Frame
;; Dada un lista de tipo Frame (trackpoints)y un numero que representa una epsilon, regresa
;; una nueva lista agrupando los deltas consecutivos, i.e., la distancia de un trackpoint a
;; otro trackpoint debe ser menor o igual a epsilon y los trackpoints deben tener el mismo
;; ritmo cardiaco
(define (collapse-trackpoint l e)
  (cond
    [(empty? l) empty]
    [(empty? (cdr l)) l]
    [else (let* ([gps1 (trackpoint-loc (car l))]
                 [gps2 (trackpoint-loc (cadr l))]
                 [h (haversine gps1 gps2)])
            (if (and (<= h e) (= (trackpoint-hr (car l)) (trackpoint-hr (cadr l))))
                (collapse-trackpoint (cdr l) e)
                (append (list (car l)) (collapse-trackpoint (cdr l) e))))]))

;; test
;(test (collapse-trackpoint empty 0.01) empty)
;(test (collapse-trackpoint tp1 0.01) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619654)))
;(test (collapse-trackpoint tp3 0.01) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655) (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658)))
;(test (collapse-trackpoint tp4 0.01) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655) (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658) (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662)))
;(test (collapse-trackpoint tp10 0.01) (list (trackpoint (GPS 19.4907258 -99.24101) 104 (resting 50 114.0) 1425619655) (trackpoint (GPS 19.4907258 -99.24101) 108 (resting 50 114.0) 1425619658) (trackpoint (GPS 19.4907107 -99.2410833) 106 (resting 50 114.0) 1425619662) (trackpoint (GPS 19.4907086 -99.2411981) 111 (resting 50 114.0) 1425619671) (trackpoint (GPS 19.4907059 -99.2412562) 112 (resting 50 114.0) 1425619675) (trackpoint (GPS 19.4906902 -99.2413796) 115 (warm-up 115.0 127.0) 1425619681) (trackpoint (GPS 19.4906865 -99.241445) 120 (warm-up 115.0 127.0) 1425619685) (trackpoint (GPS 19.4906861 -99.2415517) 119 (warm-up 115.0 127.0) 1425619690)))



; ========================================================================================
;; Sección II. Árboles Binarios
; ========================================================================================

; define arboles binarios para las pruebas
(define tree1 (bnn (bnn ebt  2 ebt) 1 (bnn ebt 3 ebt)))
(define tree2 (bnn (bnn (bnn ebt 4 ebt)  2 ebt) 1 (bnn ebt 3 ebt)))
(define tree3 (bnn (bnn (bnn ebt 4 ebt)  2 ebt) 1 (bnn (bnn ebt 5 ebt) 3 (bnn ebt 6 ebt))))
(define tree4 (BNode < (EmptyBT) 2 (BNode < (EmptyBT) 4 (EmptyBT))))
(define arbol-base1 (bns (bns (bns ebt "A" ebt) "B" (bns (bns ebt "C" ebt) "D" (bns ebt "E" ebt)))
"F"
(bns ebt "G" (bns (bns ebt "H" ebt) "I" ebt))))

;; Ejercicio 9
;; ninBT: BTree --> number
;; Dada un tipo de dato BTree, regresa el número de nodos internos
(define (ninBT t)
  (type-case BTree t
    [EmptyBT () 0]
    [BNode (c l e r) (if (or (not (EmptyBT? l)) (not (EmptyBT? r)))
                         (+ 1 (ninBT l) (ninBT r))
                         (+ (ninBT l) (ninBT r)))]))
    

;;test
;(test (ninBT (EmptyBT)) 0)
;(test (ninBT tree1) 1)
;(test (ninBT tree2) 2)
;(test (ninBT tree3) 3)
;(test (ninBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 1)



; ========================================================================================
; ========================================================================================

;; Ejercicio 10
;; nlBT: BTree --> number
;; Dada un tipo de dato BTree, regresa el número de hojas no vacías
(define (nlBT t)
  (type-case BTree t
    [EmptyBT () 0]
    [BNode (c l e r) (if (and (EmptyBT? l) (EmptyBT? r))
                         (+ 1 (nlBT l) (nlBT r))
                         (+ (nlBT l) (nlBT r)))]))

;;test
;(test (nlBT (EmptyBT)) 0)
;(test (nlBT tree1) 2)
;(test (nlBT tree2) 2)
;(test (nlBT tree3) 3)
;(test (nlBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 2)

; ========================================================================================
; ========================================================================================

;; Ejercicio 11
;; nnBT: BTree --> number
;; Dada un tipo de dato BTree, regresa el número de nodos totales que tiene
(define (nnBT t)
  (type-case BTree t
    [EmptyBT () 0]
    [BNode (c l e r) (+ 1 (nnBT l) (nnBT r))]))

;;test
;(test (nnBT (EmptyBT)) 0)
;(test (nnBT tree1) 3)
;(test (nnBT tree2) 4)
;(test (nnBT tree3) 6)
;(test (nnBT (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) 3)
;(test (nnBT maxiarb) 17)

; ========================================================================================
; ========================================================================================

;; Ejercicio 12
;; mapBT: BTree --> number
;; Dada un tipo de dato BTree, regresa el número de nodos totales que tiene
(define (mapBT p t)
  (type-case BTree t
    [EmptyBT () (EmptyBT)]
    [BNode (c l e r) (BNode c (mapBT p l) (p e) (mapBT p r))]))

;;test
;(test (mapBT add1 (EmptyBT)) (EmptyBT))
;(test (mapBT add1 tree1) (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 2 (BNode < (EmptyBT) 4 (EmptyBT))) )
;(test (mapBT sub1 tree3) (BNode < (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (EmptyBT)) 0 (BNode < (BNode < (EmptyBT) 4 (EmptyBT)) 2 (BNode < (EmptyBT) 5 (EmptyBT)))))
;(test (mapBT (lambda (x) (* 2 x)) tree4) (BNode < (EmptyBT) 4 (BNode < (EmptyBT) 8 (EmptyBT))))
;(test (mapBT (lambda (x) (* x x)) (BNode < (BNode < (EmptyBT) 3 (EmptyBT)) 1 (BNode < (EmptyBT) 2 (EmptyBT)))) (BNode < (BNode < (EmptyBT) 9 (EmptyBT)) 1 (BNode < (EmptyBT) 4 (EmptyBT))))

; ========================================================================================
; ========================================================================================

;; Ejercicio 13
;; preorderBT: BTree -> list-of-any
;; Dado un tipo de dato BTree, regresa una lista de sus elementos en pre-orden
(define (preorderBT bt)
  (type-case BTree bt
    [EmptyBT () '()]
    [BNode (c l e r) (append (list e) (preorderBT l) (preorderBT r))]))

;; test
;(test (preorderBT (EmptyBT)) '())
;(test (preorderBT tree1) '(1 2 3))
;(test (preorderBT tree2) '(1 2 4 3))
;(test (preorderBT tree3) '(1 2 4 3 5 6)) 
;(test (preorderBT arbol-base1) '("F" "B" "A" "D" "C" "E" "G" "I" "H"))

; ========================================================================================
; ========================================================================================

;; Ejercicio 14
;; inorderBT: BTree -> list-of-any
;; Dado un tipo de dato BTree, regresa una lista de sus elementos en in-orden
(define (inorderBT bt)
  (type-case BTree bt
    [EmptyBT () '()]
    [BNode (c l e r) (append (inorderBT l) (list e) (inorderBT r))]))

;; test
;(test (inorderBT (EmptyBT)) '())
;(test (inorderBT tree1) '(2 1 3))
;(test (inorderBT tree2) '(4 2 1 3))
;(test (inorderBT tree3) '(4 2 1 5 3 6))
;(test (inorderBT arbol-base1) '("A" "B" "C" "D" "E" "F" "G" "H" "I"))

; ========================================================================================
; ========================================================================================

;; Ejercicio 15
;; posorderBT: BTree -> list-of-any
;; Dado un tipo de dato BTree, regresa una lista de sus elementos en post-orden
(define (posorderBT bt)
  (type-case BTree bt
    [EmptyBT () '()]
    [BNode (c l e r) (append (posorderBT l) (posorderBT r) (list e))]))

;; test
;(test (posorderBT (EmptyBT)) '())
;(test (posorderBT tree1) '(2 3 1))
;(test (posorderBT tree2) '(4 2 3 1))
;(test (posorderBT tree3) '(4 2 5 6 3 1))
;(test (posorderBT arbol-base1) '("A" "C" "E" "D" "B" "H" "I" "G" "F"))
