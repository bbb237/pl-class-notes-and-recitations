#lang plai-typed

; This file corrects the issue in lecture5b.rkt by saving the environment *at the time of the let*
; inside of a binding.

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  )

(define-type Binding
  [bind (name : symbol) (env : Env) (defn : Expr)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Binding
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (first env)
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

; Attempt 2:
     
(define (eval-env2 (env : Env) (e : Expr)) : number
  (type-case Expr e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval-env2 env e1) (eval-env2 env e2))]
             [timesC (e1 e2) (* (eval-env2 env e1) (eval-env2 env e2))]
             [idC (x) (let ([res (lookup x env)])
                        (eval-env2 (bind-env res) (bind-defn res)))]
             [letC (x e1 e2) (eval-env2 (extend-env (bind x env e1) env) e2)]
             )
  )

(define (eval2 (e : Expr)) : number
  (eval-env2 empty-env e))


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

(eval2 (parse '(+ 27 (let x 5 (+ x x)))))
(eval2 (parse '(let x 5 (let y 6 (+ x y)))))
(eval2 (parse '(let x 5 (let x 6 (+ x x)))))

; Now these examples have behaviors like what our substitution semantics produced:
(eval2 (parse '(let x 1 (let y x (let x 2 (+ y y))))))
(eval2 (parse '(let x 1 (let y x (let z 2 (+ y y))))))
(eval2 (parse '(let x y (let y 2 (+ x x)))))

