#lang plai-typed

; Let's add let bindings

; We'll support just 1 binding at a time, e.g. a pattern like (let [(x 1)] x)
; To make the parsing simple we'll require this be written as (let x 1 x)
; Which will be represented by (letC x (numC 1) (idC x))

(define-type ArithE
  [numC (n : number)]
  [plusC (e1 : ArithE) (e2 : ArithE)]
  [timesC (e1 : ArithE) (e2 : ArithE)]
  [letC (x : symbol) (e1 : ArithE) (e2 : ArithE)]
  [idC (x : symbol)]
  )

(define (parse (s : s-expression)) : ArithE
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

(define (subst (v1 : number) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
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

(define (eval (e : ArithE)) : number
  (type-case ArithE e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval e1) (eval e2))]
             [timesC (e1 e2) (* (eval e1) (eval e2))]
             [letC (x e1 e2)
                   (let ([v1 (eval e1)])
                     (eval (subst v1 x e2)))
                   ]
             [idC (x) (error 'eval "unbound variable!")]
             )
  )

(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))
