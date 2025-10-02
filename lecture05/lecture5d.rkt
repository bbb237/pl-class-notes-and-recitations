#lang plai-typed

; Now let's develop an environment based interpreter for first-class functions (lambda)

; Once again we'll first do something that behaves "incorrectly" (or at least counter-intuitively)
; and then we'll switch to a fixed version

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  )

(define-type Value
  [numV (n : number)]
  [lambdaV (x : symbol) (e : Expr)]
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
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
       ))]
    ))

(define (val->expr (v : Value)) : Expr
  (type-case Value v
             [numV (n) (numC n)]
             [lambdaV (x e) (lambdaC x e)]))

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
             [lambdaC (x e) (lambdaV x e)]
             [plusC (e1 e2) (numV (+ (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [timesC (e1 e2) (numV (* (numV-n (eval-env env e1)) (numV-n (eval-env env e2))))]
             [letC (x e1 e2)
                   (let ([v1 (eval-env env e1)])
                     (eval-env (extend-env (bind x v1) env) e2))
                   ]
             [appC (e1 e2)
                   (let [(v1 (eval-env env e1))
                         (v2 (eval-env env e2))]
                     (eval-env (extend-env (bind (lambdaV-x v1) v2) env) (lambdaV-e v1)))]
             [idC (x) (lookup x env)]
             )
  )

(define (eval (e : Expr))
  (eval-env empty-env e))


(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))

(eval (parse '((lambda x (+ 2 x)) 1)))
(eval (parse '(let x 2
                (let addx (lambda y (+ y x))
                  (addx 3)))))

; But now...

(eval (parse '(let x 2
                (let addx (lambda y (+ y x))
                  (let x 5
                    (addx 3))))))

; Oops, we have the same problem of dynamic scope! 
