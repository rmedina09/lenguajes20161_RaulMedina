#lang plai
;1

(define (mpow a n)
  (cond 
    [(and (zero? a) (zero? n)) "valor indefinido"]
    [(zero? n) 1]
    [else (* a (mpow a (- n 1)))]))

;(test (mpow 10 0) 1)
;(test (mpow 3 2) 9)
;(test (mpow 1 20) 1)
;(test (mpow -9 2) 81)
;(test (mpow 2 8) 256)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;2 

(define (maverage l)
  (cond
    [(empty? l) 0]
    [else (/ (addElems l) (mlong l))]))

(define (mlong l)
  (cond 
    [(empty? l) 0]
    [else (+ 1 (mlong (cdr l)))]))

(define (addElems l)
  (cond 
    [(empty? l) 0]
    [else (+ (car l) (addElems (cdr l)))]))

;(test (maverage '(1 2 3 4 5)) 3)
;(test (maverage '(4)) 4)
;(test (maverage '(5 5 5 5 5 5 5)) 5)
;(test (maverage '(10 -10)) 0)
;(test (maverage '(32 4 3)) 13)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3- MPRIMES

(define (mprimes n) 
  (cond 
    [(= n 1) '()]
    [(if (isprime? n)
         (append (mprimes (sub1 n)) (list n))
         (mprimes (sub1 n)))]))

(define (isprime? n)
  (isprime2? 2 n))

(define (isprime2? k n) 
  (cond
    [(<= n 1) #f]
    [(= k n) #t]
    [else (if (= (modulo n k) 0)
         #f
         (isprime2? (add1 k) n))]))

;(test (mprimes 1) '())
;(test (mprimes 2) '(2))
;(test (mprimes 10) '(2 3 5 7))
;(test (mprimes 8) '(2 3 5 7))
;(test (mprimes 33) '(2 3 5 7 11 13 17 19 23 29 31))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;4-ZIP

(define (mzip l1 l2)
  (cond 
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (list (car l1) (car l2)) (mzip (cdr l1) (cdr l2)))]))

;(test (mzip '(1 2 3) '()) '())
;(test (mzip '() '(1 2 3)) '())
;(test (mzip '(1 2 3 4) '(1 2 3)) '((1 1) (2 2) (3 3)))
;(test (mzip '(1 2 3 4) '(1 2 3 4 5 6 7 8 9)) '((1 1) (2 2) (3 3) (4 4)))
;(test (mzip '(1 2 3 4 5 6 7 8 9) '(1 2 3 4 5)) '((1 1) (2 2) (3 3) (4 4) (5 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;5-REDUCE

(define (mreduce func l)
  (cond 
    [(< (mlong l) 2) empty]
    [(if (empty? (cddr l))
        (func (car l) (cadr l))
        (func (car l) (mreduce func (cdr l))))]))

;(test (mreduce + '(5 1 1 1 1 1)) 10)
;(test (mreduce - '(5 1 1 1 1 1)) 4)
;(test (mreduce mzip '((a b c) (a b c) (a b c))) '((a (a a)) (b (b b)) (c (c c))))
;(test (mreduce * '(1 2 3 4 5)) 120)
;(test (mreduce append '((1) (2) (3 4))) '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;6MCONCAT

(define (mconcat l1 l2)
  (cond 
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (cons (car l1) (mconcat (cdr l1) l2))]))

;(test (mconcat '() '()) '())
;(test (mconcat '(1 2 3 4) '()) '(1 2 3 4))
;(test (mconcat '() '(1 2 3 4)) '(1 2 3 4))
;(test (mconcat '(1 2 3) '(3 4)) '(1 2 3 3 4))
;(test (mconcat '(1 2 3 4) '(1 2 3 4 5)) '(1 2 3 4 1 2 3 4 5))
;(test (mconcat '(a b) '(a b)) '(a b a b))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;7-MAP
(define (mmap func l)
  (cond 
    [(empty? l) '()]
    [else (cons (func (car l)) (mmap func (cdr l)))]))

;(test (mmap sub1 '(1 1 1 1)) '(0 0 0 0))
;(test (mmap mprimes '(1 2 7 10)) '(() (2) (2 3 5 7) (2 3 5 7)))
;(test (mmap maverage '((1) (2 2) (3 3 3))) '(1 2 3))
;(test (mmap zero? '(1 1 1 0)) '(#f #f #f #t))
;(test (mmap cadr '((1 a 2) (1 b 2) (1 c 3))) '(a b c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 8 - MFILTER

(define (mfilter pred l)
  (cond 
    [(empty? l) '()]
    [(if (pred (car l))
         (cons (car l) (mfilter pred (cdr l)))
         (mfilter pred (cdr l)))]))

;(test (mfilter (lambda (l) (not (empty? l))) '((1 4 2) () (2 4) ())) '((1 4 2) (2 4)))
;(test (mfilter (lambda (x) (symbol? x)) '(a b 1 2 r 5)) '(a b r))
;(test (mfilter (lambda (n) (< n 10)) '(10 11 12 3 5 20)) '(3 5))
;(test (mfilter (lambda (y) (isprime? y)) '(1 2 5 20 10 4 23 6 7)) '(2 5 23 7))
;(test (mfilter (lambda (l) (>= (mlong l) 5)) '((1 2) () (1 2 3 4 5) (2))) '((1 2 3 4 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;9 - MANY?

(define (many? pred l)
  (cond 
    [(empty? l) #f]
    [else (or (pred (car l)) (many? pred (cdr l)))]))

;(test (many? zero? '(0 1 2 3 4 5)) #t)
;(test (many? symbol? '(0 1 2 3 4 5)) #f)
;(test (many? isprime? '(0 1 2 3 4 6)) #t)
;(test (many? symbol? '()) #f)
;(test (many? empty? '((1 2) () (2 4 5))) #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;10 - MEVERY?

(define (mevery? pred l)
  (cond 
    [(empty? l) #t]
    [else (and (pred (car l)) (mevery? pred (cdr l)))]))

;(test (mevery? zero? '()) #t)
;(test (mevery? zero? '(0 1 2 3 4 5)) #f)
;(test (mevery? isprime? '(0 1 2 3 4 5)) #f)
;(test (mevery? isprime? '(23 7 2 3 31)) #t)
;(test (mevery? symbol? '(a b c 4 e d f)) #f)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;11 - MPOWERSET

(define (mpowerset l)
  (cond 
    [(empty? l) (list '())]
    [else (append (list '()) (mpowerset-aux 1 l))]))

(define (mpowerset-aux long-set l)
  (cond     
    [(= long-set (mlong l)) (msubsets long-set l)]
    [(< long-set (mlong l)) (append (msubsets long-set l) (mpowerset-aux (add1 long-set) l))]))

(define (msubsets long-set l)
  (cond 
    [(= long-set 1) (genera 1 l)]
    [(= (mlong l) long-set) (genera long-set l)]
    [(> (mlong l) long-set) (append (genera long-set l) (msubsets long-set (cdr l)))]))


(define (genera l-subset l)
  (cond
    [(< (mlong l) l-subset) '()]
    [(= l-subset 1) (cons (list (car l)) (genera l-subset (cdr l)))]
    [else (cons (tupla l-subset l) (genera l-subset (cons (car l) (cddr l))))]))

(define (tupla l-subset l)
  (cond 
    [(<= l-subset 0) '()]
    [else (cons (car l) (tupla (sub1 l-subset) (cdr l)))]))

;(test (mpowerset '()) '(()))
;(test (mpowerset '(a)) '(() (a)))
;(test (mpowerset '(1 2)) '(() (1) (2) (1 2)))
;(test (mpowerset '(1 2 3)) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)))
;(test (mpowerset '(1 2 3 4)) '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 3 4) (2 3 4) (1 2 3 4)))












