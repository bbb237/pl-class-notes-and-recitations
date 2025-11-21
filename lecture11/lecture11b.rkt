#lang plai-typed

; This file defines a simple type checker for the language from lecture5e.rkt
; It requires annotations on letC and lambdaC to indicate the types of the variables.
; In class we discussed how it is easy to avoid requiring the type annotation on letC
; but removing the annotation on lambdaC requires a more complicated algorithm to perform
; type inference.

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (argT : Type) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (argT : Type) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  )

(define-type Type
  [numT]
  [funT (arg : Type) (ret : Type)])

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  )

(define (parse-ty (s : s-expression)) : Type
  (cond
    [(s-exp-symbol? s)
     (case (s-exp->symbol s)
       [(numT) (numT)])]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(funT) (funT (parse-ty (second l)) (parse-ty (third l)))])]))]))

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
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse-ty (third l)) (parse (fourth l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse-ty (third l)) (parse (fourth l))
                         (parse (list-ref l 4)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
       ))]
    ))

; (define (tc (e : Expr)) : boolean
;   (type-case Expr e
;              [numC #t]
;              [plusC (e1 e2) ????]
;              )

;(define (tc (e : Expr)) : Type
;  (type-case Expr e
;             [numC (n) (numT)]
;             [plusC (e1 e2) (if (and (equal? (tc e1) (numT)) (equal? (tc e2) (numT)))
;                                (numT)
;                                (error 'tc "+ not numbers"))]
;             [else (error 'tc "not covered")]
;             ))

(define (tc-env (env : TyEnv) (e : Expr)) : Type
  (type-case Expr e
             [numC (n) (numT)]
             [plusC (e1 e2) (if (and (equal? (tc-env env e1) (numT)) (equal? (tc-env env e2) (numT)))
                                (numT)
                                (error 'tc "+ not numbers"))]
             [timesC (e1 e2) (if (and (equal? (tc-env env e1) (numT)) (equal? (tc-env env e2) (numT)))
                                (numT)
                                (error 'tc "* not numbers"))]
             [lambdaC (x argT e)
                      (funT argT (tc-env (extend-env (bind x argT) env) e))]
             [idC (x)
                  (lookup x env)]
             [appC (e1 e2)
                   (type-case Type (tc-env env e1)
                              [funT (ty-arg ty-ret)
                                    (if (equal? (tc-env env e2) ty-arg)
                                        ty-ret
                                        (error 'tc "argument did not match input type"))]
                              [else (error 'tc "application of a non-function")])]
             [letC (x xT e1 e2)
                   (tc-env env (appC (lambdaC x xT e2) e1))]
;             [else (error 'tc "not covered")]
             ))


(define-type (Binding 'a)
  [bind (name : symbol) (val : 'a)])

(define-type-alias Env (listof (Binding Value)))
(define-type-alias TyEnv (listof (Binding Type)))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : (listof (Binding 'a)))) : 'a
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))
     

(define (eval-env (env : Env) (e : Expr)) : Value
  (type-case Expr e
             [numC (n) (numV n)]
             [lambdaC (x argT e) (closV env x e)]
             [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [letC (x xT e1 e2)
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
             )
  )

(define (eval (e : Expr))
  (eval-env empty-env e))


(eval (parse '(+ 27 (let x numT 5 (+ x x)))))
(eval (parse '(let x numT 5 (let y numT 6 (+ x y)))))
(eval (parse '(let x numT 5 (let x numT 6 (+ x x)))))
(eval (parse '(let x numT 5 (let x numT (* x 2) (+ x x)))))
(parse '(+ 27 (let x numT 5 (+ x x))))
(parse '(let x numT 5 (let y numT 6 (+ x y))))
(parse '(let x numT 5 (let x numT 6 (+ x x))))
(parse '(let x numT 5 (let x numT (* x 2) (+ x x))))

(define (tc (e : Expr))
  (tc-env empty-env e))

;(parse '(let x numT 2
;                (let addx (funT numT numT) (lambda y numT (+ y x))
;                   (addx 3))))
; 
; ; And now:
; 
(tc (parse '(let x numT 2
                (let addx (funT numT numT) (lambda y numT (+ y x))
                  (let x numT 6
                    (addx 3))))))

; It gives 5, as we'd expect with lexical scope

(tc (parse '(lambda x numT (+ 2 x))))
(tc (parse '((lambda x numT (+ 2 x)) 1)))

(tc (parse '(lambda f (funT numT numT) (f 3))))
(tc (parse '((lambda f (funT numT numT) (f 3)) (lambda x numT (+ x 3)))))
