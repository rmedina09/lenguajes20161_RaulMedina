#lang plai

(require "practica4-base.rkt")

;(print-only-errors true)

;====================================================================
;Funciones Auxiliares
;====================================================================
;; list-name: listof-Binding-FAES --> listof-symbol
;; Dada una lista de Bindings-FAES regresa una lista con solo los nombres de los bindings
(define (list-name lbind)
  (cond
    [(empty? lbind) empty]
    [else (type-case Binding (car lbind)
            [bind (name val) (cons name (list-name (cdr lbind)))])]))

;; list-value: listof-Binding-FAES --> listof-FAE
;; Dada una lista de Bindings-FAES regresa una lista con solo los valores-FAE de los bindings
(define (list-value lbind)
  (cond
    [(empty? lbind) empty]
    [else (type-case Binding (car lbind)
            [bind (name val) (cons (desugar val) (list-value (cdr lbind)))])]))

;; list-fae: listof-FAES --> listof-FAE
;; Dada una lista de FAES regresa una lista de FAE
(define (list-fae l)
  (cond
    [(empty? l) empty]
    [else (cons (desugar (car l)) (list-fae (cdr l)))]))

;; list-Value-App: listof-Binding-FAES --> listof-FAE-app
;;
(define (list-Value-App lb)
  (cond
    [(empty? lb) empty]
    [else (type-case Binding (car lb)
            [bind (name val) (cons (app (fun (list name) (id name)) (list (desugar val))) (list-Value-App (cdr lb)))])]))

(define lbds (list (bind 'x (numS 3)) (bind 'y (numS 6))))
;(test (list-name lbds) '(x y))
;(test (list-value lbds) (list (numS 3) (numS 6)))
;(test (parse '{with {{x {+ 5 5}}} x}) (withS (list (bind 'x (binopS + (numS 5) (numS 5)))) (idS 'x)))


;====================================================================
;====================================================================

(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [withS (bindings body) (app (fun (list-name bindings) (desugar body)) (list-value bindings))]
    [with*S (bindings body) (app (fun (list-name bindings) (desugar body)) (list-Value-App bindings))]
    [idS (name) (id name)]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (list-fae args))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]))
  
;;Test
;(test (desugar (parse 3)) (num 3))
;(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
;(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
;(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5)))))
;(test (desugar (parse '{with* {{x {+ 5 5}} {y 7}} {+ x y}})) (app (fun '(x y) (binop + (id 'x) (id 'y))) (list (binop + (num 5) (num 5)) (num 7))))

;=====================================================================================
;=====================================================================================

(define (cparse sexp)
  (desugar (parse sexp)))

;=====================================================================================
;; Funciones Auxiliares
;=====================================================================================

;; binop-value: procedure FAE-Value FAE-Value --> FAE-Value
;; Dado un operador binario (+ - * /), y dos FAE-Value regresa la evaluación del operador
;; con las dos FAE-Value como sus parametros
(define (binop-value op n1 n2)
  (numV (op (numV-n n1) (numV-n n2))))

;; lookup: symbol Env --> FAE-Value
;;
(define (lookup var e)
  (type-case Env e
    [mtSub () (error 'lookup "free identifier")]
    [aSub (name value env) (if (symbol=? var name)
                               value
                               (lookup var env))]))


(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [id (name) (lookup name env)]
    [fun (params body) (closureV params body env)]
    [app (fun args) (let* ([fun-val (interp fun env)]
                           [params-fun (closureV-param fun-val)]
                           [args-val (get-argsV args env)]
                           [e (get-env (reverse params-fun) (reverse args-val) env)])
                      (interp (closureV-body fun-val) e))]
    [binop (f l r) (binop-value f (interp l env) (interp r env))]))

  ;; Implementar interp
  ;; (error 'interp "Not implemented"))

;; get-argsV: listof-FAE Env --> listof-FAE-Value
;;
(define (get-argsV lfae e)
  (cond
    [(empty? lfae) empty]
    [else (cons (interp (car lfae) e) (get-argsV (cdr lfae) e))]))

;(test (get-argsV (list (binop + (num 5) (num 5)) (num 7)) (mtSub)) (list (numV 10) (numV 7)))

;; get-env: listof-symbol listof-FAE-Value Env --> Env
;;
(define (get-env pf argV e)
  (cond
    [(or (empty? pf) (empty? argV)) e]
    [else (aSub (car pf) (car argV) (get-env (cdr pf) (cdr argV) e))]))

;(test (get-env '(x y) (list (numV 10) (numV 7)) (mtSub)) (aSub 'x (numV 10) (aSub 'y (numV 7) (mtSub))))
;(test (get-env '(x y) (list (numV 10) (numV 7)) (aSub 'z (numV 20) (mtSub))) (aSub 'x (numV 10) (aSub 'y (numV 7) (aSub 'z (numV 20) (mtSub)))))

(define (rinterp expr)
  (interp expr (mtSub)))

;(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "lookup: free identifier")
;(test (rinterp (cparse '3)) (numV 3))
;(test (rinterp (cparse '{+ 3 4})) (numV 7))
;(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
;(test (rinterp (cparse '{with {{x {+ 5 5}}} x})) (numV 10))
;(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
;(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
;(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
;(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
;(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
;(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
;(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
;(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
;(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
;(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
;(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
;(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
;(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
;(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
;(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))