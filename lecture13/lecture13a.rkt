#lang plai-typed
(require (typed-in racket/base [random : (number -> number)]))

(define-type Expr
  [numC (n : number)]
  [boolC (b : boolean)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [equal?C (e1 : Expr) (e2 : Expr)]
  [ifC (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [recC (f : symbol) (x : symbol) (e : Expr)]
  [nondetC]
  )

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [recV (env : Env) (f : symbol) (x : symbol) (e : Expr)]
  )

(define (parse (s : s-expression)) : Expr
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-boolean? s) (boolC (s-exp->boolean s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (cond
         [(s-exp-symbol? (first l))
          (case (s-exp->symbol (first l))
            [(+) (plusC (parse (second l)) (parse (third l)))]
            [(*) (timesC (parse (second l)) (parse (third l)))]
            [(equal?) (equal?C (parse (second l)) (parse (third l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(nondet) (nondetC)]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
            [(if) (ifC (parse (second l)) (parse (third l)) (parse (fourth l)))]
            [(rec) (recC (s-exp->symbol (second l)) (s-exp->symbol (third l)) (parse (fourth l)))]
            [else (appC (parse (first l)) (parse (second l)))]
            )]
         [else (appC (parse (first l)) (parse (second l)))]
       ))]
    ))


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
     

(define (mjoin (ls : (listof (listof 'a)))) : (listof 'a)
  (foldr append empty ls))

(define (mbind (ls : (listof 'a)) (f : ('a -> (listof 'b)))) : (listof 'b)
  (mjoin (map f ls)))

(define (eval-env (env : Env) (e : Expr)) : (listof Value)
  (type-case Expr e
             [numC (n) (list (numV n))]
             [boolC (b) (list (boolV b))]
             [lambdaC (x e) (list (closV env x e))]
             [recC (f x e) (list (recV env f x e))]
             [plusC (e1 e2)
                    (mbind (eval-env env e1)
                           (lambda (v1)
                             (mbind (eval-env env e2)
                                    (lambda (v2)
                                      (list (numV (+ (numV-n v1) (numV-n v2))))))))]
             [timesC (e1 e2)
                    (mbind (eval-env env e1)
                           (lambda (v1)
                             (mbind (eval-env env e2)
                                    (lambda (v2)
                                      (list (numV (* (numV-n v1) (numV-n v2))))))))]
             [equal?C (e1 e2)
                    (mbind (eval-env env e1)
                           (lambda (v1)
                             (mbind (eval-env env e2)
                                    (lambda (v2)
                                      (list (boolV (equal? v1 v2)))))))]
             [ifC (guard e1 e2)
                  (mbind (eval-env env guard)
                         (lambda (vguard)
                           (if (boolV-b vguard)
                               (eval-env env e1)
                               (eval-env env e2))))]
             [letC (x e1 e2)
                   (mbind (eval-env env e1)
                          (lambda (v1)
                            (eval-env (extend-env (bind x v1) env) e2)))]
             [appC (e1 e2)
                    (mbind (eval-env env e1)
                           (lambda (v1)
                             (mbind (eval-env env e2)
                                    (lambda (v2)
                                      (type-case Value v1
                                                 [closV (env x e)
                                                        (eval-env (extend-env (bind x v2) env) e)]
                                                 [recV (env f x e)
                                                       (eval-env (extend-env (bind f v1)
                                                                             (extend-env (bind x v2) env)) e)]
                                                 [else (error 'env "Apply non-function")])))))]
             [nondetC () (list (boolV #f) (boolV #t))]
             [idC (x) (list (lookup x env))]
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

; And now:

(eval (parse '(let x 2
                (let addx (lambda y (+ y x))
                  (let x 6
                    (addx 3))))))

(eval (parse '(if #f 2 3)))
(eval (parse '(if #t 2 3)))

(eval (parse '(let b 1
                (let fact
                  (rec fact x
                    (if (equal? x 0)
                        b
                        (* x (fact (+ x -1)))))
                  (let b 2 (fact 5))))))

(eval (parse '(+ 1 (if (nondet) 0 1))))

(eval (parse '(let maybe-sum
                  (rec maybe-sum x
                    (if (nondet)
                        (if (equal? x 0)
                            0
                            (+ x (maybe-sum (+ x -1))))
                        0))
                  (maybe-sum 5))))

;(eval (parse '(let subset-sum
;                  (rec subset-sum x
;                       (let v (if (nondet) x 0)
;                          (if (equal? x 0)
;                            0
;                            (+ v (subset-sum (+ x -1))))))
;                  (subset-sum 5))))

(eval (parse '(let subset-sum
                  (rec subset-sum x
                          (if (equal? x 0)
                            0
                            (+ (if (nondet) 0 x) (subset-sum (+ x -1)))))
                  (subset-sum 5))))

;(eval (parse '(if (nondet) 1 x)))
