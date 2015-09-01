#lang plai
(define (mpow a n)
  (cond 
    [(zero? n) 1]
    [else (* a (mpow a (- n 1)))]))

;;(test (mpow 3 2) 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MPRIMES

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


;(define (isdivisor? n m) 
 ; (if (= (modulo n m) 0)
 ;     #t
      ;#f))
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ZIP

(define (mzip l1 l2)
  (cond 
    [(or (empty? l1) (empty? l2)) '()]
    [else (cons (list (car l1) (car l2)) (mzip (cdr l1) (cdr l2)))]))

;(test (mzip '(1 2 3) '()) '())
;(test (mzip '() '(1 2 3)) '())
;(test (mzip '(1 2 3 4) '(1 2 3)) '((1 1) (2 2) (3 3)))
;(test (mzip '(1 2 3 4) '(1 2 3 4 5 6 7 8 9)) '((1 1) (2 2) (3 3) (4 4)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;REDUCE
(define (mreduce func l)
  (cond 
    [(< (mlong l) 2) empty]
    [(if (empty? (cddr l))
        (func (car l) (cadr l))
        (func (car l) (mreduce func (cdr l))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MCONCAT
(define (mconcat l1 l2)
  (cond 
    [(and (empty? l1) (empty? l2)) '()]
    [(empty? l1) l2]
    [(empty? l2) l1]
    [else (cons (car l1) (mconcat (cdr l1) l2))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MMAP
(define (mmap func l)
  (cond 
    [(empty? l) '()]
    [else (cons (func (car l)) (mmap func (cdr l)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MFILTER
(define (mfilter pred l)
  (cond 
    [(empty? l) '()]
    [(if (pred (car l))
         (cons (car l) (mfilter pred (cdr l)))
         (mfilter pred (cdr l)))]))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MANY?
(define (many? pred l)
  (cond 
    [(empty? l) #f]
    [else (or (pred (car l)) (many? pred (cdr l)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MEVERY?
(define (mevery? pred l)
  (cond 
    [(empty? l) #t]
    [else (and (pred (car l)) (mevery? pred (cdr l)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MPOWERSET
;(define (mpowerset l)
 ; (cond 











































































