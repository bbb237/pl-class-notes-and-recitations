#lang plai-typed
(require (typed-in racket/base [gensym : (-> symbol)]))

#|

In this file, we add lambdas (like in Racket) to our interpreter that
had eager (non-lazy) treatment of lets.

We'll support just lambdas that take 1 argument at a time.  To make
the parsing simple we'll write these as (lambda x e), instead of
requiring (lambda (x) e) as Racket requires.
|#

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [lambdaC (x : symbol) (e : Expr)] ; This case represents a lambda declaration (lambda x e)
  [appC (e1 : Expr) (e2 : Expr)] ; This represents applying the function e1 to e2
  )

#|
Now our expressions might not just evaluate to a number. They can also
evaluate to a lambda.  i.e. if we ask to evaluate (lambdaC x e), then
just like in Racket we can't really "run" this function so we just say
it returns back a value which is that function.
|#

(define-type Value
  [numV (n : number)]
  [lambdaV (x : symbol) (e : Expr)])

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
             [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
             [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
             [else (appC (parse (first l)) (parse (second l)))]
             )]
         [else (appC (parse (first l)) (parse (second l)))])
       )]
    ))

; We can convert values into expressions

(define (value->expr (v : Value)) : Expr
  (type-case Value v
    [numV (n) (numC n)]
    [lambdaV (x e) (lambdaC x e)]))

; Now we need to substitute not just numbers but values into expressions.

(define (possibly-free? (z : symbol) (e : Expr)) : boolean
  #t)

; For doing capture avoiding substitution we will want to rename
; variables to a different variable, so it is useful here if subst again lets you substitute an expression
; instead of just values
(define (subst (erep : Expr) (x : symbol) (e : Expr)) : Expr
  (type-case Expr e
             [idC (y) (if (equal? x y) erep (idC y))]
             [numC (n) (numC n)]
             [plusC (e1 e2) (plusC (subst erep x e1) (subst erep x e2))]
             [timesC (e1 e2) (timesC (subst erep x e1) (subst erep x e2))]
             [letC (y e1 e2)
                   (cond
                     ; If x = y, then we don't want to substitute into e2, since
                     ; occurences of x in e2 are bound and referring to this let definition
                     [(equal? x y) (letC y (subst erep x e1) e2)]

                     ; If y might be free in erep, then we need to avoid occurrences of y in erep becoming
                     ; captured. To do so, we will generate a fresh variable z using gensym,
                     ; and replace y with z in the let, *then* do the substitution
                     [(possibly-free? y erep)
                      (let [(z (gensym))]
                        (letC z (subst erep x e1) (subst erep x (subst (idC z) y e2))))]

                     ; If y is not free in erep, then we don't need to do this renaming.
                     ; (though the possibly-free? function above always returns true)
                     [else
                        (letC y (subst erep x e1) (subst erep x e2))])
                   ]
             [appC (e1 e2)
                   (appC (subst erep x e1) (subst erep x e2))]
             [lambdaC (y e)
                    ; Similarly, when substituting into the lambda, we use
                    ; gensym to come up with a fresh variable name for the lambda's argument
                    ; and replace it to avoid capturing a free variable.
                    ; Note: just like with letC, we could use possibly-free? to avoid
                    ; unnecessarily renaming.
                   (cond
                     [(equal? x y) (lambdaC y e)]
                     [else
                      (let [(z (gensym))]
                        (lambdaC z (subst erep x (subst (idC z) y e))))])
                   ]
             )
  )

(define (eval (e : Expr)) : Value
  (type-case Expr e
             [numC (n) (numV n)]
             [plusC (e1 e2)
                    (numV (+ (numV-n (eval e1)) (numV-n (eval e2))))]
             [timesC (e1 e2)
                    (numV (* (numV-n (eval e1)) (numV-n (eval e2))))]
             [letC (x e1 e2)
                   (let ([v1 (eval e1)])
                     (eval (subst (value->expr v1) x e2)))
                   ]
             [idC (x) (error 'eval "unbound variable!")]
             [appC (e1 e2)
                   ; Evaluate e1, get out some lambda, evaluate e2 get some value v,
                   ; let's say e1 evaluates to (lambda x e')
                   ; substitute v for x in e', recursively evaluate the result
                   (let [(v1 (eval e1))
                         (v2 (eval e2))]
                     (eval (subst (value->expr v2) (lambdaV-x v1) (lambdaV-e v1))))]
             [lambdaC (x e)
                      (lambdaV x e)]
             )
  )


; Our old examples that just involve let.
(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))

; Some new examples with lambdas.
(eval (parse '((lambda f (f (f 1))) (lambda x (* 2 x)))))
(eval (parse '(let do-twice (lambda f (f (f 1)))
          (let double (lambda x (* 2 x))
           (do-twice double)))))

; Now we can see that the buggy example from the previous file
; correctly raises an exception

(test/exn (eval (parse '(let f (lambda x (+ x y))
                  (let y 6
                   (f 1))))) "eval: unbound variable")

; Indeed, we can see how the argument in lambda gets renamed as part of the substitution of y
(eval (parse '(let f (lambda x (+ x y))
                  (let y 6
                   f))))

; When I tested this, it returned
;
; > (lambdaV 'g3507 (plusC (idC 'g3507) (idC 'y)))
;
; here the 'g3507 is the gensym generated symbol
; (when you run it you might get a different value instead of 'g3507)

