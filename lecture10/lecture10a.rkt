#lang plai-typed

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [tryC (handler : Expr) (body : Expr)]
  [raiseC (e : Expr)]
  )

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
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
            [(try) (tryC (parse (second l)) (parse (third l)))]
            [(raise) (raiseC (parse (second l)))]
            [(lambda) (lambdaC (s-exp->symbol (second l)) (parse (third l)))]
            [(let) (letC (s-exp->symbol (second l)) (parse (third l)) (parse (fourth l)))]
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
     

(define-type Contin
  [haltK]
  [plusLK (e : Expr) (env : Env) (k : Contin)]
  [plusRK (v : Value) (env : Env) (k : Contin)]
  [timesLK (e : Expr) (env : Env) (k : Contin)]
  [timesRK (v : Value) (env : Env) (k : Contin)]
  [appLK (e : Expr) (env : Env) (k : Contin)]
  [appRK (v : Value) (env : Env) (k : Contin)]
  [tryLK (body : Expr) (env : Env) (k : Contin)]
  [tryRK (handler : Value) (env : Env) (k : Contin)]
  [raiseK (env : Env) (k : Contin)]
  )

(define-type ExnContin
  [haltEK]
  [tryEK (handler : Value) (env : Env) (k : Contin) (kexn : ExnContin)])

(define (apply-exncontin kexn (v : Value))
  (type-case ExnContin kexn
             [haltEK () (error 'apply-exncontin "Uncaught raise.")]
             [tryEK (handler env k kexn)
                    (eval-env (extend-env (bind (closV-x handler) v)
                                          (closV-env handler)) (closV-e handler) k kexn)]))

(define (apply-contin (k : Contin) (kexn : ExnContin) (v : Value)) : Value
  (type-case Contin k
             [haltK () v]
             [plusLK (e2 env k)
                     (eval-env env e2 (plusRK v env k) kexn)]
             [plusRK (v1 env k)
                     (apply-contin k kexn (numV (+ (numV-n v1) (numV-n v))))]
             [timesLK (e2 env k)
                     (eval-env env e2 (timesRK v env k) kexn)]
             [timesRK (v1 env k)
                     (apply-contin k kexn (numV (* (numV-n v1) (numV-n v))))]
             [appLK (e2 env k)
                    (eval-env env e2 (appRK v env k) kexn)]
             [appRK (v1 env k)
                    (eval-env (extend-env (bind (closV-x v1) v) (closV-env v1)) (closV-e v1) k kexn)]
             [tryLK (body env k)
                    (eval-env env body (tryRK v env k) (tryEK v env k kexn))]
             [tryRK (handler env k)
                    (apply-contin k kexn v)]
             [raiseK (env k) (apply-exncontin kexn v)]
             ;[else (error 'apply-contin "Not yet implemented")]
             ))

(define (eval-env (env : Env) (e : Expr) (k : Contin) (kexn : ExnContin)) : Value
  (type-case Expr e
             [numC (n) (apply-contin k kexn (numV n))]
             [plusC (e1 e2)
                    (eval-env env e1 (plusLK e2 env k) kexn)]
             [timesC (e1 e2)
                    (eval-env env e1 (timesLK e2 env k) kexn)]
             [lambdaC (x e) (apply-contin k kexn (closV env x e))]
             [appC (e1 e2)
                   (eval-env env e1
                             (appLK e2 env k)
                             kexn)]
             [letC (x e1 e2)
                   (eval-env env (appC (lambdaC x e2) e1) k kexn)]
             [idC (x) (apply-contin k kexn (lookup x env))]
             [raiseC (e)
                   (eval-env env e (raiseK env k) kexn)]
             [tryC (handler body)
                   (eval-env env handler (tryLK body env k) kexn)]
             )
  )

(define (eval (e : Expr))
  (eval-env empty-env e (haltK) (haltEK)))

(define-syntax-rule (test-equal-num? a b) (test a (numV b)))

(test-equal-num? (eval (parse '27)) 27)
(test-equal-num? (eval (parse '(+ 1 1))) 2)
(test-equal-num? (eval (parse '(+ (+ 1 2) 3))) 6)
(test-equal-num? (eval (parse '(* (+ 1 2) 3))) 9)

(test-equal-num? (eval (parse '(+ 27 (let x 5 (+ x x))))) 37)
(test-equal-num? (eval (parse '(let x 5 (let y 6 (+ x y))))) 11)
(test-equal-num? (eval (parse '(let x 5 (let x 6 (+ x x))))) 12)
(test-equal-num? (eval (parse '(let x 5 (let x (* x 2) (+ x x))))) 20)

(test-equal-num? (eval (parse '((lambda x (+ 2 x)) 1))) 3)
(test-equal-num? (eval (parse '(let x 2
                                 (let addx (lambda y (+ y x))
                                   (addx 3))))) 5)

; And now:

(test-equal-num?
 (eval (parse '(let x 2
                (let addx (lambda y (+ y x))
                  (let x 6
                    (addx 3))))))
 5)

(test-equal-num? (eval (parse '(try (lambda x (+ x 6)) 142))) 142)
(test-equal-num? (eval (parse '(try (lambda x (+ x 6)) (raise 142)))) 148)
(test-equal-num? (eval (parse '(try (lambda x (+ x 6)) (raise (raise 5))))) 11)

(test-equal-num?
 (eval (parse
       '(let f (try (lambda x (+ 10 x))
                    (lambda y (raise 1)))
          (try (lambda x (+ 20 x))
               (f 1)))))
 21)

(test/exn (eval (parse '(raise 1))) "Uncaught")

; (parse '(try (lambda x (+ x 6)) (raise 1)))
; (parse '(try (lambda x (+ x 6)) (raise (+ 6 7))))
