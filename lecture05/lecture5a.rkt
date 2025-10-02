#lang plai-typed

; We revisit just simple eager let-bindings where the only value
; in the language is a number.

; However, instead of using substitution, we will use environments as an implementation strategy.

; We'll support just 1 binding at a time, e.g. a pattern like (let [(x 1)] x)
; To make the parsing simple we'll require this be written as (let x 1 x)
; Which will be represented by (letC x (numC 1) (idC x))

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  )

; An environment will be a data structure that stores a mapping
; from variables to values that those variables should have.
; We'll just represent that as, essentially, a simple list of pairs of variables with numbers.

(define-type Binding
  [bind (name : symbol) (val : number)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : number
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-val (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))
     
(define (eval-env (env : Env) (e : Expr)) : number
  (type-case Expr e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval-env env e1) (eval-env env e2))]
             [timesC (e1 e2) (* (eval-env env e1) (eval-env env e2))]
             [idC (x) (lookup x env)]
             [letC (x e1 e2)
                   (let ([v1 (eval-env env e1)])
                     (eval-env (extend-env (bind x v1) env) e2))
                   ]
             )
  )

(define (eval (e : Expr)) : number
  (eval-env empty-env e))

(define default-env : Env
  (extend-env (bind 'pi 3.14159) (extend-env (bind 'e 2.71828) empty-env)))

(define (eval-default-env (e : Expr)) : number
  (eval-env default-env e))

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (case (s-exp->symbol (first l))
         [(+) (plusC (parse (second l)) (parse (third l)))]
         [(*) (timesC (parse (second l)) (parse (third l)))]
         [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
         )
       )]
    ))


; Our old substitution evaluator for comparison

(define (subst (v1 : number) (x : symbol) (e : Expr)) : Expr
  (type-case Expr e
             [idC (y) (if (equal? x y) (numC v1) (idC y))]
             [numC (n) (numC n)]
             [plusC (e1 e2) (plusC (subst v1 x e1) (subst v1 x e2))]
             [timesC (e1 e2) (timesC (subst v1 x e1) (subst v1 x e2))]
             [letC (y e1 e2)
                   (let ([e2 (if (equal? x y) e2 (subst v1 x e2))])
                     (letC y (subst v1 x e1) e2))
                   ]
             )
  )

(define (eval-subst (e : Expr)) : number
  (type-case Expr e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval-subst e1) (eval-subst e2))]
             [timesC (e1 e2) (* (eval-subst e1) (eval-subst e2))]
             [letC (x e1 e2)
                   (let ([v1 (eval-subst e1)])
                     (eval-subst (subst v1 x e2)))
                   ]
             [idC (x) (error 'eval-subst "unbound variable!")]
             )
  )

(eval-subst (parse '(+ 27 (let x 5 (+ x x)))))
(eval-subst (parse '(let x 5 (let y 6 (+ x y)))))
(eval-subst (parse '(let x 5 (let x 6 (+ x x)))))
(eval-subst (parse '(let x 5 (let x (* x 2) (+ x x)))))

(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))

(eval-default-env (parse '(let r 10 (* pi (* r r)))))
