#lang plai

;(define (checklong l n)
;  (if (<= (length l) n)
;      #t
;      #f))

;(define (longitud l)
;  (type-case MList l
;    [MCons (cabeza cola) (+ 1 (longitud cola))]
;    [MEmpty () 0]))

(define-type Array
  [MArray (msize number?) (marr list?)])


(define-type MList
  [MCons (mhead number?) (mrest MList?)]
  [MEmpty])

(define-type NTree
  [TLEmpty]
  [NodeN (elem number?) (sons (listof NTree?))])

(define-type Position
  [2D-Point (cX number?) (cY number?)])

(define-type Figure
  [Circle (center Position?) (radio number?)]
  [Square (corner Position?) (heigth number?)]
  [Rectangle (corner Position?) (width number?) (heigth number?)])

;=================================================================

