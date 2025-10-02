#lang plai-typed

; Let's tweak the environment based interpreter to do lazy execution.

; We'll take a first attempt in this file, but it will lead to slightly confusing program behaviors.
; See lecture5c.rkt for an alternate version that avoids these issues.

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  )

(define-type Binding
  [bind (name : symbol) (defn : Expr)])

(define-type-alias Env (listof Binding))
(define empty-env empty)
(define extend-env cons)

(define (lookup (x : symbol) (env : Env)) : Expr
  (cond
    [(cons? env)
     (if (equal? (bind-name (first env)) x)
         (bind-defn (first env))
         (lookup x (rest env)))]
    [else (error 'lookup "No binding found")]))

; Attempt 1:
     
(define (eval-env1 (env : Env) (e : Expr)) : number
  (type-case Expr e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval-env1 env e1) (eval-env1 env e2))]
             [timesC (e1 e2) (* (eval-env1 env e1) (eval-env1 env e2))]
             [idC (x) (eval-env1 env (lookup x env))]
             [letC (x e1 e2) (eval-env1 (extend-env (bind x e1) env) e2)]
             )
  )

(define (eval1 (e : Expr)) : number
  (eval-env1 empty-env e))


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

(eval1 (parse '(+ 27 (let x 5 (+ x x)))))
(eval1 (parse '(let x 5 (let y 6 (+ x y)))))
(eval1 (parse '(let x 5 (let x 6 (+ x x)))))
; So far so good.

(eval1 (parse '(let x 1 (let y x (let x 2 (+ y y))))))

; We get 4, but under our substitution semantics this would give us 2.

; Indeed, if we rename the inner variable x to z, we get 2 again.
(eval1 (parse '(let x 1 (let y x (let z 2 (+ y y))))))

; Other odd behaviors, similar to the example we saw with capturing substitution:
(eval1 (parse '(let x y (let y 2 (+ x x)))))

; This is called *dynamic* scope, whereas the treatment of variables
; we looked at previously is called lexical or static scope.

; Dynamic here means that the binding/definition that the variable refers to
; is a property of the dynamic execution history of the program. Whereas a static/lexical
; scope means that the binding site / definition of a variable can be determined by
; inspecting the program without executing it. (This is slightly informal, we could give
; a more precise definition. )

