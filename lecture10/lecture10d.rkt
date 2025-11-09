#lang plai-typed

(define-type Expr
  [numC (n : number)]
  [plusC (e1 : Expr) (e2 : Expr)]
  [timesC (e1 : Expr) (e2 : Expr)]
  [letC (x : symbol) (e1 : Expr) (e2 : Expr)]
  [let/ccC (x : symbol) (e1 : Expr)]
  [lambdaC (x : symbol) (e : Expr)]
  [appC (e1 : Expr) (e2 : Expr)]
  [idC (x : symbol)]
  [tryC (handler : Expr) (body : Expr)]
  [raiseC (e : Expr)]
  )

(define-type Value
  [numV (n : number)]
  [closV (env : Env) (x : symbol) (e : Expr)]
  [continV (k : ((Value -> Value) Value -> Value))]
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
            [(let/cc) (let/ccC (s-exp->symbol (second l)) (parse (third l)))]
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
     

(define-type-alias Contin (ExnContin Value -> Value))
(define-type-alias ExnContin (Value -> Value))
(define (apply-contin (k : Contin) (kexn : ExnContin) (v : Value)) : Value
  (k kexn v))

(define (haltK) : Contin
  (lambda (kexn v) v))

(define (plusLK e2 env k) : Contin
  (lambda (kexn v) (eval-env env e2 (plusRK v env k) kexn)))

(define (plusRK v1 env k) : Contin
  (lambda (kexn v) (apply-contin k kexn (numV (+ (numV-n v1) (numV-n v))))))

(define (timesLK e2 env k) : Contin
  (lambda (kexn v) (eval-env env e2 (timesRK v env k) kexn)))

(define (timesRK v1 env k) : Contin
  (lambda (kexn v) (apply-contin k kexn (numV (* (numV-n v1) (numV-n v))))))

(define (appLK e2 env k) : Contin
  (lambda (kexn v) (eval-env env e2 (appRK v env k) kexn)))

(define (appRK v1 env k) : Contin
  (lambda (kexn v)
    (type-case Value v1
               [numV (n) (error 'bad "Bad app")]
               [closV (env x e) (eval-env (extend-env (bind x v) env) e k kexn)]
               [continV (ksaved) (apply-contin ksaved kexn v)])))

(define (tryLK body env k)
  (lambda (kexn v) 
    (eval-env env body (tryRK v env k) (tryEK v env k kexn))))

(define (tryRK handler env k)
  (lambda (kexn v) (apply-contin k kexn v)))

(define (raiseK env k)
  (lambda (kexn v) (apply-exncontin kexn v)))

(define (apply-exncontin kexn (v : Value))
  (kexn v))

(define (haltEK) : ExnContin
  (lambda (v) (error 'halteEK "Uncaught Raise.")))

(define (tryEK handler env k kexn) : ExnContin
  (lambda (v)
    (eval-env (extend-env (bind (closV-x handler) v)
                          (closV-env handler)) (closV-e handler) k kexn)))
  
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
             [let/ccC (x e)
                      (eval-env (extend-env (bind x (continV k)) env) e k kexn)]
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

(eval (parse '(+ 1 (let/cc k (k 3)))))
(eval (parse '(+ 1 (let/cc k (+ 2 (let/cc j (+ 10 (j 99))))))))

; (parse '(try (lambda x (+ x 6)) (raise 1)))
; (parse '(try (lambda x (+ x 6)) (raise (+ 6 7))))
