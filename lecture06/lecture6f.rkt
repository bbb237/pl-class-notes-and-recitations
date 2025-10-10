#lang plai-typed

;; This file revisits lecture6c.rkt and re-writes the interpreter using a programming
;; technique called "monads". This cleans up the managing of the store.

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [boxC (e : Expr)]
  [unboxC (e : Expr)]
  [setboxC (e1 : Expr) (e2 : Expr)]
  [seqC (e1 : Expr) (e2 : Expr)]
  )

(define-type-alias Location number)
 
(define-type Storage
  [cell (location : Location) (val : Value)])
 
(define-type-alias Store (listof Storage))
(define empty-store empty)
(define override-store cons)

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [boxV (l : Location)]
  )


(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(begin) (seqC (parse (second l)) (parse (third l)))]
            [(box) (boxC (parse (second l)))]
            [(set-box!) (setboxC (parse (second l)) (parse (third l)))]
            [(unbox) (unboxC (parse (second l)))]
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

(define (fetch (l : Location) (sto : Store)) : Value
  (cond
    [(cons? sto)
     (if (equal? (cell-location (first sto)) l)
         (cell-val (first sto))
         (fetch l (rest sto)))]
    [else (error 'fetch "No location found")]))

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

(define-type (Result 'a)
  [res (v : 'a) (s : Store)])

(define-type-alias (Stateful 'a)
  (Store -> (Result 'a)))

(define (mret (v : 'a)) : (Stateful 'a)
  (lambda (s) (res v s)))

(define (mbind (m : (Stateful 'a)) (f : ('a -> (Stateful 'b)))) : (Stateful 'b)
  (lambda (s1) 
    (type-case (Result 'a) (m s1)
               [res (v s2) ((f v) s2)])))

(define (mseq (m1 : (Stateful 'a)) (m2 : (Stateful 'b))) : (Stateful 'b)
  (mbind m1
         (lambda (v) m2)))

(define (moverride (l : Location) (v : Value)) : (Stateful Value)
  (lambda (s)
    (res (boxV l) (override-store (cell l v) s))))

(define (mfetch (l : Location)) : (Stateful Value)
  (lambda (s)
    (res (fetch l s) s)))

(define new-loc
  (let ([counter (box 0)])
    (lambda () 
      (let ([l (unbox counter)])
        (begin (set-box! counter (+ 1 l))
               l)))))

     
(define (eval-env (env : Env) (e : Expr)) : (Stateful Value)
  (type-case Expr e
             [numC (n) (mret (numV n))]
             [lambdaC (x e) (mret (closV env x e))]
             [plusC (e1 e2)
                      (mbind (eval-env env e1)
                             (lambda (v1)
                               (mbind (eval-env env e2)
                                      (lambda (v2)
                                        (mret (numV (+ (numV-n v1) (numV-n v2))))))))]
             [timesC (e1 e2)
                      (mbind (eval-env env e1)
                             (lambda (v1)
                               (mbind (eval-env env e2)
                                      (lambda (v2)
                                        (mret (numV (* (numV-n v1) (numV-n v2))))))))]
             [letC (x e1 e2) (eval-env env (appC (lambdaC x e2) e1))]
             [appC (e1 e2)
                   (mbind (eval-env env e1)
                          (lambda (v1)
                            (mbind (eval-env env e2)
                                   (lambda (v2)
                                     (eval-env (extend-env (bind (closV-x v1) v2) (closV-env v1))
                                               (closV-e v1))))))]
             [seqC (e1 e2)
                   (mbind (eval-env env e1)
                          (lambda (v1)
                            (eval-env env e2)))]
             [idC (x) (mret (lookup x env))]
             [boxC (a) (mbind (eval-env env a)
                              (lambda (v)
                                (let [(l (new-loc))]
                                  (mseq (moverride l v)
                                        (mret (boxV l))))))]
             [unboxC (a) (mbind (eval-env env a)
                                (lambda (v)
                                  (mfetch (boxV-l v))))]
             [setboxC (e1 e2)
                      (mbind (eval-env env e1)
                             (lambda (v1)
                               (mbind (eval-env env e2)
                                      (lambda (v2)
                                        (moverride (boxV-l v1) v2)))))]
             ))

(define (eval (e : Expr)) : (Result Value)
  ((eval-env empty-env e) empty-store))

(define ex0 (parse '(let v (+ 1 1) (+ v v))))
(define ex1 (parse '(begin (+ 1 1) (+ 2 2))))
(define ex2 (parse '(let b (box 0) (set-box! b 1))))
(define ex3 (parse '(let b (box 0) (unbox b))))
(define ex4 (parse '(let b (box 0) (begin (begin (set-box! b 1)
                                     (set-box! b (+ 1 (unbox b))))
                              (unbox b)))))
(define ex5
  (parse '(let a (box 1)
            (let f (lambda x (+ x (unbox a)))
              (begin
                (set-box! a 2)
                (f 10))))))
