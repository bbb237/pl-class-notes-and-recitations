#lang plai-typed

; Lazy let bindings with substitution

; Rather than executing (letC x e1 e2) by first running e1 to a value,
; and substituting the value, we'll substitute the whole expression
; e1.


; We import the gensym function from base racket, which
; generates a fresh symbol that has never been used before
(require (typed-in racket/base [gensym : (-> symbol)]))

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

#| We now need to substitute *expressions* in for variables, instead
of just substituting in numbers. This leads to a new subtlety.  This
is how you might try at first to define this substitution but this has
a problem that if erep is an open term (i.e. has free variables) then
those free variables can become bound when substituted in; these
variables are said to be "captured".  |#

; (define (subst (erep : ArithE) (x : symbol) (e : ArithE)) : ArithE
;   (type-case ArithE e
;              [idC (y) (if (equal? x y) erep (idC y))]
;              [numC (n) (numC n)]
;              [plusC (e1 e2) (plusC (subst erep x e1) (subst erep x e2))]
;              [timesC (e1 e2) (timesC (subst erep x e1) (subst erep x e2))]
;              [letC (y e1 e2)
;                    (let ([e2 (if (equal? x y) e2 (subst erep x e2))])
;                      (letC y (subst erep x e1) e2))
;                    ]
;              )
;   )

#|

Consider: 

(let x y (let y 2 (+ x y)))
(let x y (let z 2 (+ x z)))

These two expressions should really behave the same, because all we
have done is renamed y into z in the inner let. Indeed, under our
interpreter for last week with the eager (i.e. non-lazy) semantics,
these would both give an unbound variable error.

However, with the above buggy definition of substitution, when we
substitute y for x in the inner let, with the above code, the first
one will result in (let y 2 (+ y y), whereas the second one will be
(let z 2 (+ y z)). The should then eventually evaluate to 4, while the
latter would give an unbound variable.

This behavior is bad! Because it means that somehow the names we pick
for a variable affect the outcome if they somehow happen to collide
with a variable name used in a different part of the program. As we
said our previous "eager" substitution semantics didn't have that
issue.

We can avoid this issue using so-called "capture avoiding"
substitution which renames variables to avoid causing capture.

i.e., effectively when trying to substitute in the first example

(let x y (let y 2 (+ x y)))

when it gets to the inner let, it will "rename" the variable y in that
let binding so that it can't possibly conflict with the varible y that
we are going to substitute in.

|#

; For fun : try implementing "possibly-free" correctly so that you
; only rename when needed above.

(define (possibly-free? (z : symbol) (e : ArithE)) : boolean
  #t)

(define (subst (erep : ArithE) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
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
             )
  )


(define (eval (e : ArithE)) : number
  (type-case ArithE e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval e1) (eval e2))]
             [timesC (e1 e2) (* (eval e1) (eval e2))]
             [letC (x e1 e2)
                     (eval (subst e1 x e2))
                   ]
             [idC (x) (error 'eval "unbound variable!")]
             )
  )

(eval (parse '(+ 27 (let x 5 (+ x x)))))
(eval (parse '(let x 5 (let y 6 (+ x y)))))
(eval (parse '(let x 5 (let x 6 (+ x x)))))
(eval (parse '(let x 5 (let x (* x 2) (+ x x)))))


; Now we can see that for the example above, we get unbound variable properly
; in both cases, even when we rename y to z.

(test/exn (eval (parse '(let x y (let y 2 (+ x y))))) "eval: unbound variable")
(test/exn (eval (parse '(let x y (let z 2 (+ x z))))) "eval: unbound variable")


; Note: another way we could have avoided this issue with capture would
; be to first check that the program has no unbound variables, and raise
; an error even before execution then, our substitution would only ever
; substite expressions that had no free variables.

; Still, the idea of capture avoiding substitution is important for
; other reasons and shows up in other situations where there is not such
; an easy work around, which is why I have explained it here.
