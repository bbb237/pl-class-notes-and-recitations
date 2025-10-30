#lang plai-typed

#|

Starting point for our implementation of a simple macro expander.
This file takes our evaluator for a simple language with lambda, and
adds constructors to the AST for defining macros with let-syntaxC and
generating syntax with syntaxC. The parser has been extended to
support these but the expander/evaluator do not yet.

|#

(define-type Expr
  [numC (n : number)]
  [boolC (b : boolean)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [idC (x : symbol)]
  [equalC (e1 : Expr) (e2 : Expr)]
  [ifC (e1 : Expr) (e2 : Expr) (e3 : Expr)]
  [appC (e : Expr) (eargs : (listof Expr))]
  [syntaxC (e : Expr)]
  [let-syntaxC (x : symbol) (args : (listof symbol)) (edef : Expr) (body : Expr)]
  )

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (env : (Env Value)) (x : symbol) (e : Expr)]
  )

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equalC (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(syntax) (syntaxC (parse (second l)))]
            [(let-syntax) (let-syntaxC (s-exp->symbol (second l))
                                       (map s-exp->symbol (s-exp->list (third l)))
                                       (parse (fourth l))
                                       (parse (fourth (rest l)))
                                       )]
            [else (appC (parse (first l)) (map parse (rest l)))]
            )]
         [else (appC (parse (first l)) (map parse (rest l)))]
       ))]
    ))

(define-type (Binding 't)
  [bind (name : symbol) (val : 't)])

(define-type-alias (Env 't) (listof (Binding 't)))
(define empty-env empty)
(define extend-env cons)
(define append-env append)

(define (lookup (x : symbol) (env : (Env 't))) : 't
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup (string-append "No binding found for " (symbol->string x)))]))
     
(define (lookup-default (x : symbol) (env : (Env 't)) (default : 't)) : 't
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup-default x (rest env) default))]
    [else default]))

(define (eval-env (env : (Env Value)) (e : Expr)) : Value
  (type-case Expr e
             [numC (n) (numV n)]
             [boolC (b) (boolV b)]
             [lambdaC (x e) (closV env x e)]
             [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [letC (x e1 e2)
                   (let ([v1 (eval-env env e1)])
                     (eval-env (extend-env (bind x v1) env) e2))
                   ]
             [appC (e1 es)
                   (let [(v1 (eval-env env e1))
                         (v2 (eval-env env (first es)))]
                     (eval-env (extend-env (bind (closV-x v1) v2) (closV-env v1)) (closV-e v1)))]
             [idC (x) (lookup x env)]
             [equalC (e1 e2) (boolV (equal? (eval e1) (eval e2)))]
             [ifC (guard e1 e2)
                  (if (equal? (eval-env env guard) (boolV #t))
                      (eval-env env e1)
                      (eval-env env e2))]
             [else (error 'eval "not implemented")]
             )
  )


(define (expand e) : Expr
  (error 'expand "not implemented"))

(define (eval e) : Value
  (eval-env empty (expand e)))

#|
(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))

(eval (parse '((lambda x (+ 2 x)) 1)))
(eval (parse '(let x 2
                (let addx (lambda y (+ y x))
                  (addx 3)))))

(eval (parse '(let x 2
                (let addx (lambda y (+ y x))
                  (let x 6
                    (addx 3))))))

(eval (parse '(if #t 1 2)))
(eval (parse '(if #f 1 2)))
|#


(define my-or1 (parse '(let-syntax my-or (e1 e2) (syntax (if e1 #t e2)) (my-or #t (1 1)))))

;(expand my-or1)
;(eval my-or1)
;(display "\n")

(define my-let1 (parse '(let-syntax my-let (q e1 e2) (syntax ((lambda q e2) e1))
                                    (my-let x 1 (my-let y 2 (+ x y))))))
;(expand my-let1)
;(eval my-let1)
;(display "\n")

(define nested (parse '
                (let-syntax my-or (e1 e2) (syntax (if e1 #t e2))
                            (let-syntax my-let (q e1 e2) (syntax ((lambda q e2) e1))
                                        (my-let x (my-or #f #f) (my-let y #f (my-or x y)))))))

;(expand nested)
;(eval nested)
;(display "\n")


(define ex1-scope (parse
               '(let-syntax plus1 (e1) (syntax (let t 1 (+ t e1)))
                            (let x 5 (plus1 x)))))

; Unhygenic: t gets captured by the let t in the definition of the macro.

(define ex2-scope (parse
               '(let-syntax plus1 (e1) (syntax (let t 1 (+ t e1)))
                            (let t 5 (plus1 t)))))
