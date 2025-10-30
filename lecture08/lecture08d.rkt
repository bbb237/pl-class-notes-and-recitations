#lang plai-typed

#|

This fills lecture08c.rkt to add the implementation of the expander.

NOTE: this implementation of macros is pretty limited/brittle in
various ways.  For example, it does not implement hygenic
macros. Additionally, it does not allow you to actually do
case-analysis / pattern match on syntax. The point is not to overly
study this particular implementation, but to help make more concrete
the idea of a separation between expansion and evaluation.

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
  [syntaxC (e : Expr)]
  [appC (e : Expr) (eargs : (listof Expr))]
  [let-syntaxC (x : symbol) (args : (listof symbol)) (edef : Expr) (body : Expr)]
  )

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (env : (Env Value)) (x : symbol) (e : Expr)]
  [mclosV (env : (Env Value)) (args : (listof symbol)) (edef : Expr)]
  [syntaxV (e : Expr)]
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

(define (eval-macro env args edef es) : Expr
  (let [(packed (map2 (lambda (x e) (bind x (syntaxV e))) args es))]
    (type-case Value (eval-env (append packed env) edef)
               [syntaxV (e) e]
               [else (error 'eval-macro "Macro did not return syntax")])))
  

(define (expand-symbol (env : (Env Value)) (x : symbol)) : symbol
  (type-case Value (lookup-default x env (syntaxV (idC x)))
             [syntaxV (e) (idC-x e)]
             [else (error 'expand-symbol "Didn't resolve to symbol")]))
                       

(define (expand-env (env : (Env Value)) (e : Expr)) : Expr
  (type-case Expr e
             [numC (n) (numC n)]
             [boolC (b) (boolC b)]
             [plusC (e1 e2) (plusC (expand-env env e1) (expand-env env e2))]
             [timesC (e1 e2) (timesC (expand-env env e1) (expand-env env e2))]
             [let-syntaxC (name args edef ebody)
                          (expand-env (extend-env (bind name (mclosV env args edef)) env) ebody)]
             [letC (x e1 e2)
                   (let [(x-new (expand-symbol env x))]
                     (letC x-new
                           (expand-env env e1)
                           (expand-env (extend-env (bind x-new (syntaxV (idC x-new))) env) e2)))]
             [lambdaC (x e)
                      (let [(x-new (expand-symbol env x))]
                        (lambdaC x-new
                                 (expand-env (extend-env (bind x-new (syntaxV (idC x-new))) env) e)))]
             [appC (e1 es)
                   (type-case Expr e1
                              [idC (x)
                                   (type-case Value (lookup x env)
                                              [syntaxV (e) (appC (expand-env env e)
                                                                 (map (lambda (e) (expand-env env e)) es))]
                                              [mclosV (clo-env args def)
                                                      (expand-env env (eval-macro clo-env args def es))]
                                              [else (error 'expand-env "bad application")])]
                              [else (appC (expand-env env e1) (map (lambda (e) (expand-env env e)) es))])]
             [idC (x)
                  (type-case Value (lookup x env)
                             [syntaxV (e) e]
                             [else (error 'expand-env "Lookup didn't resolve to syntax")])]
             [equalC (e1 e2) (equalC (expand-env env e1) (expand-env env e2))]
             [ifC (guard e1 e2)
                  (ifC (expand-env env guard) (expand-env env e1) (expand-env env e2))]
             [syntaxC (e) (syntaxC e)]))

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
             [syntaxC (e) (syntaxV (expand-env env e))]
             [else (error 'eval "should not evaluate a let-syntax in eval")]
             )
  )


(define (expand e) : Expr
  (expand-env empty e))

(define (eval e) : Value
  (eval-env empty (expand-env empty e)))

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

(expand my-or1)
(eval my-or1)
(display "\n")

(define my-let1 (parse '(let-syntax my-let (q e1 e2) (syntax ((lambda q e2) e1))
                                    (my-let x 1 (my-let y 2 (+ x y))))))
(expand my-let1)
(eval my-let1)
(display "\n")

; A strange variant of "let" in which the definition of the variable is a number and is doubled
(define my-let-double (parse '(let-syntax my-let-double (q e1 e2)
                                          (syntax (let q (* 2 e1) e2))
                                    (my-let-double x 2 (my-let-double y 4 (+ x y))))))
(expand my-let-double)
(eval my-let-double)
(display "\n")

; Another variant of my-let-double
(define my-let-double-alt (parse '(let-syntax my-let-double (q e1 e2)
                                          (syntax (let q e1 (let q (+ q q) e2)))
                                    (my-let-double x 2 (my-let-double y 4 (+ x y))))))
(expand my-let-double-alt)
(eval my-let-double-alt)
(display "\n")

(define nested (parse '
                (let-syntax my-or (e1 e2) (syntax (if e1 #t e2))
                            (let-syntax my-let (q e1 e2) (syntax ((lambda q e2) e1))
                                        (my-let x (my-or #f #f) (my-let y #f (my-or x y)))))))

(expand nested)
(eval nested)
(display "\n")


(define ex1-scope (parse
               '(let-syntax plus1 (e1) (syntax (let t 1 (+ t e1)))
                            (let x 5 (plus1 x)))))

; Unhygenic: t gets captured by the let t in the definition of the macro.

(define ex2-scope (parse
               '(let-syntax plus1 (e1) (syntax (let t 1 (+ t e1)))
                            (let t 5 (plus1 t)))))
