#lang plai-typed
(require (typed-in racket/base [random : (number -> number)]))

(define-type Expr
  [numC (n : number)]
  [boolC (b : boolean)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [equal?C (e1 : Expr) (e2 : Expr)]
  [ifC (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [recC (f : symbol) (x : symbol) (e : Expr)]
  [boxC (e : Expr)]
  [unboxC (e : Expr)]
  [setboxC (e1 : Expr) (e2 : Expr)]
  [seqC (e1 : Expr) (e2 : Expr)]
  )

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [recV (env : Env) (f : symbol) (x : symbol) (e : Expr)]
  [boxV (l : Location)]
  )

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(begin) (seqC (parse (second l)) (parse (third l)))]
            [(box) (boxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(unbox) (unboxC (parse (second l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(rec) (recC (s-exp->symbol (second l)) (s-exp->symbol (third l)) (parse (fourth l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
       ))]
    ))


(define-type Binding
  [bind (name : symbol) (val : Value)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))
     

; (define (mjoin (ls : (listof (listof 'a)))) : (listof 'a)
;   (foldr append empty ls))
; 
; (define (mbind (ls : (listof 'a)) (f : ('a -> (listof 'b)))) : (listof 'b)
;   (mjoin (map f ls)))
; 
; (define (mreturn (x : 'a)) : (listof 'a)
;   (list x))
;
; (define-type-alias (M 'a) (listof 'a))

(define-type (Result 'a)
  [res (v : 'a) (s : Store)])

(define-type-alias (M 'a)
  (Store -> (Result 'a)))

(define-type-alias Location number)
 
(define-type Storage
  [cell (location : Location) (val : Value)])

(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)

(define (fetch (l : Location) (sto : Store)) : Value
  (cond
    [(cons? sto)
     (if (equal? (cell-location (first sto)) l)
         (cell-val (first sto))
         (fetch l (rest sto)))]
    [else (error 'fetch "No location found")]))

(define (mreturn (v : 'a)) : (M 'a)
  (lambda (s) (res v s)))

(define (mbind (m : (M 'a)) (f : ('a -> (M 'b)))) : (M 'b)
  (lambda (s1) 
    (type-case (Result 'a) (m s1)
               [res (v s2) ((f v) s2)])))

(define (moverride (l : Location) (v : Value)) : (M Value)
  (lambda (s)
    (res (boxV l) (override-store (cell l v) s))))

(define (mfetch (l : Location)) : (M Value)
  (lambda (s)
    (res (fetch l s) s)))

(define new-loc
  (let ([counter (box 0)])
    (lambda () 
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))


(define-syntax mlet
  (syntax-rules ()
    [(mlet ([name e]) body)
     (mbind e (lambda (name) body))]
    [(mlet ([name e] rem ...) body)
     (mbind e (lambda (name) (mlet (rem ...) body)))]))

(define (eval-env (env : Env) (e : Expr)) : (M Value)
  (type-case Expr e
             [numC (n) (mreturn (numV n))]
             [boolC (b) (mreturn (boolV b))]
             [lambdaC (x e) (mreturn (closV env x e))]
             [recC (f x e) (mreturn (recV env f x e))]
             [plusC (e1 e2)
                    (mlet ([v1 (eval-env env e1)]
                           [v2 (eval-env env e2)])
                          (mreturn (numV (+ (numV-n v1) (numV-n v2)))))]
             [timesC (e1 e2)
                    (mlet ([v1 (eval-env env e1)]
                           [v2 (eval-env env e2)])
                          (mreturn (numV (* (numV-n v1) (numV-n v2)))))]
             [equal?C (e1 e2)
                    (mlet ([v1 (eval-env env e1)]
                           [v2 (eval-env env e2)])
                          (mreturn (boolV (equal? v1 v2))))]
             [ifC (guard e1 e2)
                  (mlet ([vguard (eval-env env guard)])
                        (if (boolV-b vguard)
                            (eval-env env e1)
                            (eval-env env e2)))]
             [letC (x e1 e2)
                   (mbind (eval-env env e1)
                          (lambda (v1)
                            (eval-env (extend-env (bind x v1) env) e2)))]
             [appC (e1 e2)
                    (mlet ([v1 (eval-env env e1)]
                           [v2 (eval-env env e2)])
                          (type-case Value v1
                                     [closV (env x e)
                                            (eval-env (extend-env (bind x v2) env) e)]
                                     [recV (env f x e)
                                           (eval-env (extend-env (bind f v1)
                                                                 (extend-env (bind x v2) env)) e)]
                                     [else (error 'env "Apply non-function")]))]
             [boxC (a) (mlet ([v (eval-env env a)]
                              [l (mreturn (new-loc))]
                              [x (moverride l v)])
                             (mreturn (boxV l)))]
             [unboxC (a) (mlet ([v (eval-env env a)])
                               (mfetch (boxV-l v)))]
             [setboxC (e1 e2)
                      (mlet ([v1 (eval-env env e1)]
                             [v2 (eval-env env e2)])
                            (moverride (boxV-l v1) v2))]
             [seqC (e1 e2)
                   (mlet ([v1 (eval-env env e1)])
                         (eval-env env e2))]
             [idC (x) (mreturn (lookup x env))]
            )
 )

(define (eval (e : Expr))
  ((eval-env empty-env e) empty-store))

(define ex5
  (parse '(let a (box 1)
            (let f (lambda x (+ x (unbox a)))
              (begin
                (set-box! a 2)
                (f 10))))))

;(eval ex5)
