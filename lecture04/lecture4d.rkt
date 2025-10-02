#lang plai-typed
(require (typed-in racket/base [gensym : (-> symbol)]))

#| We can also implement lambdas so that function application behaves
in a lazy way.  That is, when we have an application ((lambda x e1)
e2), rather than immediately evaluating e2, we can substitute e2 in
for x in e1.  |#

#| This file does that, having both lazy lets and lazy lambdas |#

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

(define (possibly-free? (z : symbol) (e : Expr)) : boolean
  #t)

(define (val->expr (v : Value)) : Expr
  (type-case Value v
             [numV (n) (numC n)]
             [lambdaV (x e) (lambdaC x e)]))

(define (subst (v : Expr) (x : symbol) (e : Expr)) : Expr
  (type-case Expr e
             [idC (y) (if (equal? x y) v (idC y))]
             [numC (n) (numC n)]
             [plusC (e1 e2) (plusC (subst v x e1) (subst v x e2))]
             [timesC (e1 e2) (timesC (subst v x e1) (subst v x e2))]
             [appC (e1 e2) (appC (subst v x e1) (subst v x e2))]
             [lambdaC (y e)
                   (cond
                     [(equal? x y) (lambdaC y e)]
                     [else
                      (let [(z (gensym))]
                        (lambdaC z (subst v x (subst (idC z) y e))))])
                   ]
             [letC (y e1 e2)
                   (cond
                     [(equal? x y) (letC y (subst v x e1) e2)]
                     [(possibly-free? y v)
                      (let [(z (gensym))]
                        (letC z (subst v x e1) (subst v x (subst (idC z) y e2))))]
                     [else
                        (letC y (subst v x e1) (subst v x e2))])
                   ]
             )
  )


(define (eval (e : Expr)) : Value
  (type-case Expr e
             [numC (n) (numV n)]
             [lambdaC (x e) (lambdaV x e)]
             [plusC (e1 e2) (numV (+ (numV-n (eval e1)) (numV-n (eval e2))))]
             [timesC (e1 e2) (numV (* (numV-n (eval e1)) (numV-n (eval e2))))]
             [letC (x e1 e2) (eval (subst e1 x e2))]
             [appC (e1 e2)
                   ; This is the key difference from the eager lambdas in the previous file:
                   ; we don't evaluate e2 and just immediately substitute e2
                   (let [(v1 (eval e1))]
                     (eval (subst e2 (lambdaV-x v1) (lambdaV-e v1))))]
             [idC (x) (error 'eval "unbound variable!")]
             )
  )

(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))


(test/exn (eval (parse '(let x y (let y 2 (+ x y))))) "eval: unbound variable")
(test/exn (eval (parse '(let x y (let z 2 (+ x z))))) "eval: unbound variable")

(parse '((lambda f (f (f 1))) (lambda x (* 2 x))))
(eval (parse '((lambda f (f (f 1))) (lambda x (* 2 x)))))
(parse '(let do-twice (lambda f (f (f 1)))
          (let double (lambda x (* 2 x))
           (do-twice double))))
(eval (parse '(let do-twice (lambda f (f (f 1)))
          (let double (lambda x (* 2 x))
           (do-twice double)))))

(test/exn (eval (parse '(let f (lambda x (+ x y))
                          (let y 2
                            (f (+ 1 2)))))) "eval: unbound variable")
