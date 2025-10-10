#lang plai-typed

;; This file is an *incorrect* implementation of boxes that tries to
;; implement them by just including boxes as a value in the environment.
;; The examples at the end show how this fails to give the right behavior.

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

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [boxV (v : Value)]
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

(define (lookup (x : symbol) (env : Env)) : Value
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))
     
(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
             [numC (n) (numV n)]
             [lambdaC (x e) (closV env x e)]
             [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [letC (x e1 e2)
                   (let ([v1 (eval-env env e1)])
                     (eval-env (extend-env (bind x v1) env) e2))
                   ]
             [appC (e1 e2)
                   (let [(v1 (eval-env env e1))
                         (v2 (eval-env env e2))]
                     ; Now when we run the body of the function, we're going to use and extend
                     ; the saved environment from the closure, instead of the current environment
                     (eval-env (extend-env (bind (closV-x v1) v2) (closV-env v1)) (closV-e v1)))]
             [idC (x) (lookup x env)]
             [boxC (a) (boxV (eval-env env a))]
             [unboxC (a) (boxV-v (eval-env env a))]
             [seqC (e1 e2) (let ([v (eval-env env e1)])
                             (eval-env env e2))]
             [setboxC (e1 e2) (let ([v (eval-env env e1)])
                                (boxV (eval-env env e2)))]
             )
  )

(define (eval (e : Expr))
  (eval-env empty-env e))

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
