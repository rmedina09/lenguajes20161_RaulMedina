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

;; list-value: listof-Binding --> listof-FAE
;; Dada una lista de Bindings regresa una lista con solo los valores-FAE de los bindings
(define (list-value lbind)
  (cond
    [(empty? lbind) empty]
    [else (type-case Binding (car lbind)
            [bind (name val) (cons (desugar val) (list-value (cdr lbind)))])]))

;; lfaes-value: listof-Bindings --> listof-FAES
;; Dada una lista de bindings, obtiene de cada elemento de la lista original el valor del
;; binding y lo devuelve en una nueva lista 
(define (lfaes-value lbind)
  (cond
    [(empty? lbind) empty]
    [else (type-case Binding (car lbind)
            [bind (name val) (cons val (lfaes-value (cdr lbind)))])]))


;; list-fae: listof-FAES --> listof-FAE
;; Dada una lista de FAES regresa una lista de FAE
(define (list-fae l)
  (cond
    [(empty? l) empty]
    [else (cons (desugar (car l)) (list-fae (cdr l)))]))

;; desugar-args: symbol listof-bindings --> FAES
;; Dado un simbolo y  una lista de bindings, busca el binding tal que si su "name" y el simbolo son iguales, regresa su valor.
(define (get-value s lb)
  (cond
    [(empty? lb) (error 'desugar-args "no tiene valor")]
    [else (type-case Binding (car lb)
            [bind (name val) (if (symbol=? s name) val (get-value s (cdr lb)))])]))

;; cut-list: listof-Binding symbol --> listof-Binding 
;; Dada una lista de bingings y un simbolo, regresa la lista a partir del primer binding
;; tal que el "name" del binding es igual al simbolo.(Se usa para mantener el alcance en el desugar de with*)
(define (cut-list lb s)
  (cond
    [(empty? lb) empty]
    [else (type-case Binding (car lb)
            [bind (n v) (if (symbol=? n s) lb (cut-list (cdr lb) s))])]))

;; list-Value-App: FAES listof-Binding --> listof-FAE
;; Dada una expresión de tipo FAE y una lista de bindings regresa la traducción de la expresión original a una sintaxis FAE
;; donde los casos particulares son para binop y idS solo hace una traducción por medio de "app" y "binop", para el resto de
;; los FAES utiliza la función desugar
(define (value-app fs lb)
  (type-case FAES fs
            [idS (n) (app (fun (list n) (id n)) (list (value-app (get-value n (cut-list lb n)) (cut-list lb n))))]
            [binopS (f l r) (binop f (value-app l lb) (value-app r lb))]
            [else (desugar fs)]))

;; list-value-app: listof-Binding --> listof-FAE
;; Dada una lista de bindings, regresa una nueva lista donde cada elemento es
;; el "desugar" a FAE, en si busca los "idS" para devolver una traducción a "app"
(define (list-value-app lv lb laux)
  (cond
    [(empty? lv) empty]
    [else (cons (value-app (car lv) laux) (list-value-app (cdr lv) (cdr lb) (cons (car lb) laux)))]))

  

;(define lbds (list (bind 'x (numS 3)) (bind 'y (binopS + (numS 2) (idS 'x)))))
;(define lv (list (numS 3) (binopS + (numS 2) (idS 'x))))
;(test (list-name lbds) '(x y))
;(test (list-value lbds) (list (numS 3) (numS 6)))
;(test (parse '{with {{x {+ 5 5}}} x}) (withS (list (bind 'x (binopS + (numS 5) (numS 5)))) (idS 'x)))


;====================================================================
;====================================================================
;; desugar: FAES --> FAE
(define (desugar expr)
  (type-case FAES expr
    [numS (n) (num n)]
    [withS (bindings body) (app (fun (list-name bindings) (desugar body)) (list-value bindings))]
    [with*S (bindings body) (app (fun (list-name bindings) (desugar body)) (list-value-app (lfaes-value bindings) bindings (list (car bindings))))]
    [idS (name) (id name)]
    [funS (params body) (fun params (desugar body))]
    [appS (fun args) (app (desugar fun) (list-fae args))]
    [binopS (f l r) (binop f (desugar l) (desugar r))]))
  
;;Test
;(test (desugar (parse 3)) (num 3))
;(test (desugar (parse '{+ 3 4})) (binop + (num 3) (num 4)))
;(test (desugar (parse '{+ {- 3 4} 7})) (binop + (binop - (num 3) (num 4)) (num 7)))
;(test (desugar (parse '{with {{x {+ 5 5}}} x})) (app (fun '(x) (id 'x)) (list (binop + (num 5) (num 5)))))

;=====================================================================================
;=====================================================================================

;; A::= <number>|<symbol>|listof(<A>)
;; parse: A -> FAES
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
;; Dado un simbolo y un ambiente, busca el valor de una variable, representada por el simbolo,
;; en el ambiente y devuelve su valor
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
                           [args-val (get-argsV args env)])                           
                      (type-case FAE-Value fun-val
                        [closureV (param body e) (interp (closureV-body fun-val) (get-env (reverse params-fun) (reverse args-val) e))]
                        [else (error 'interp "can only apply functions")]))]
    [binop (f l r) (binop-value f (interp l env) (interp r env))]))


;=======================================================================
; Funciones auxiliares para interp
;=======================================================================

;; get-argsV: listof-FAE Env --> listof-FAE-Value
;; Dada una lista de FAE y un ambiente, regresa una lista donde cada elemento
;; es el interp de cada elemento de la lista-FAE original 
(define (get-argsV lfae e)
  (cond
    [(empty? lfae) empty]
    [else (cons (interp (car lfae) e) (get-argsV (cdr lfae) e))]))

;(test (get-argsV (list (binop + (num 5) (num 5)) (num 7)) (mtSub)) (list (numV 10) (numV 7)))

;; get-env: listof-symbol listof-FAE-Value Env --> Env
;; Dada una lista de simbolos, una lista de FAE-Value y un ambiente, regresa la construcción
;; de un ambiente nuevo, que consta del ambiente que pasan como parametro y de lo que se obtiene
;; de emparejar las dos listas. Este nuevo ambiente se construye para el interp de app
(define (get-env pf argV e)
  (cond
    [(or (empty? pf) (empty? argV)) e]
    [else (aSub (car pf) (car argV) (get-env (cdr pf) (cdr argV) e))]))

;(test (get-env '(x y) (list (numV 10) (numV 7)) (mtSub)) (aSub 'x (numV 10) (aSub 'y (numV 7) (mtSub))))
;(test (get-env '(x y) (list (numV 10) (numV 7)) (aSub 'z (numV 20) (mtSub))) (aSub 'x (numV 10) (aSub 'y (numV 7) (aSub 'z (numV 20) (mtSub)))))

;=======================================================================
;=======================================================================


(define (rinterp expr)
  (interp expr (mtSub)))

(test (rinterp (cparse '{with* {{x 3} {y {+ x x}}} y})) (numV 6))
(test/exn (rinterp (cparse '{{fun {x y} y} 3 {+ 2 x}})) "lookup: free identifier")
(test (rinterp (cparse '3)) (numV 3))
(test (rinterp (cparse '{+ 3 4})) (numV 7))
(test (rinterp (cparse '{+ {- 3 4} 7})) (numV 6))
(test (rinterp (cparse '{with {{x {+ 5 5}}} x})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {+ x x}})) (numV 10))
(test (rinterp (cparse '{with {{x {+ 5 5}}} {with {{y {- x 3}}} {+ y y}}})) (numV 14))
(test (rinterp (cparse '{with {{x 5} {y {- 5 3}}} {+ x y}})) (numV 7))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} 10}}})) (numV 15))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{x 3}} x}}})) (numV 8))
(test (rinterp (cparse '{with {{x 5}} {+ x {with {{y 3}} x}}})) (numV 10))
(test (rinterp (cparse '{with {{x 5}} {with {{y x}} y}})) (numV 5))
(test (rinterp (cparse '{with {{x 5}} {with {{x x}} x}})) (numV 5))
(test (rinterp (cparse '{{fun {x} x} 3})) (numV 3))
(test (rinterp (cparse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (numV 8))
(test (rinterp (cparse '{with {{x 3}} {fun {y} {+ x y}}})) (closureV '(y) (binop + (id 'x) (id 'y)) (aSub 'x (numV 3) (mtSub))))
(test (rinterp (cparse '{with {{x 10}} {{fun {y} {+ y x}} {+ 5 x}}})) (numV 25))
(test (rinterp (cparse '{with {{x 1} {y 2} {z 3}} {+ {+ x y} z}})) (numV 6))
(test (rinterp (cparse '{{fun {x y z} {+ {+ x y} z}} 1 2 3})) (numV 6))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {z {+ x y}}} z})) (numV 8))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {z {+ x y}}} z})) (numV 15))
(test (rinterp (cparse '{with* {{x 3} {y {+ 2 x}} {x 10} {y 10} {z {+ x y}}} z})) (numV 20))
(test (rinterp (cparse '{with* {{x 1} {y {+ 2 x}} {x 10} {z {+ x y}} {y 10}} z})) (numV 13))
(test/exn (rinterp (cparse '{with {{x 10} {x 20}} x})) "El id x está repetido")
(test (rinterp (cparse '{with* {{x 10} {x 20}} x})) (numV 20))