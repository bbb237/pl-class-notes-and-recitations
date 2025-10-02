#lang plai-typed

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


(define (subst (v1 : Value) (x : symbol) (e : Expr)) : Expr
  (type-case Expr e
             [idC (y) (if (equal? x y) (value->expr v1) (idC y))]
             [numC (n) (numC n)]
             [plusC (e1 e2) (plusC (subst v1 x e1) (subst v1 x e2))]
             [timesC (e1 e2) (timesC (subst v1 x e1) (subst v1 x e2))]
             [letC (y e1 e2)
                   (let ([e2 (if (equal? x y) e2 (subst v1 x e2))])
                     (letC y (subst v1 x e1) e2))
                   ]
             [appC (e1 e2)
                   (appC (subst v1 x e1) (subst v1 x e2))]
             [lambdaC (y e)
                      ; Just like with let, we need to check whether
                      ; y (the arguement to the lambda) is equal to x,
                      ; the variable we are trying to substitute for.
                      ; if it is, then we *don't* want to do substitution into
                      ; the body of e, because occurrences of x in e refer
                      ; to the argument of the function. If they're not equal,
                      ; then we recursively substitute.
                    (lambdaC y (if (equal? x y) e (subst v1 x e)))]
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
                     (eval (subst v1 x e2)))
                   ]
             [idC (x) (error 'eval "unbound variable!")]
             [appC (e1 e2)
                   ; Evaluate e1, get out some lambda, evaluate e2 get some value v,
                   ; let's say e1 evaluates to (lambda x e')
                   ; substitute v for x in e', recursively evaluate the result
                   (let [(v1 (eval e1))
                         (v2 (eval e2))]
                     (eval (subst v2 (lambdaV-x v1) (lambdaV-e v1))))]
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

; Unfortunately, this code has a bug related to capture. Consider the following example:
(eval (parse '(let f (lambda x (+ x y))
                  (let y 6
                   (f 1)))))

; * NOTE: in class unfortunately I had a typo in this example, forgetting to put the parenthesis *
; * around the let, so it was triggering an error unrelated to the point I wanted to make        *

#|

What should happen with this example? I claim that sensibly it ought
to produce an unbound variable error: in the definition of f, the
variable y is free and has no definition, so when the function f is
applied to the argument 1, we should expect that it becomes (+ 1 y),
which would trigger an unbound variable error.  Indeed, if you write
the corresponding racket program, it will do so.  However, our current
interpreter will actually return 7. Why?

When it substitutes (lambda x (+ x y)) in for f, it will end up with

(let y 6
  ((lambda x (+ x y)) 1)

which then evaluates to 7 because the unbound y has become captured by the let!

The issue is that, again, just like with the substitution for lazy
lets, now that we are substituting lambdas instead of just numbers, if
those lambdas have free variables, we have to worry about capture.

Check out lecture4c.rkt for a fix.

|#
