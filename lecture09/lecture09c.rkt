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

(define (find-handler (v : Value) (k : Contin))
  (type-case Contin k
             [tryRK (handler env k)
                    (eval-env (extend-env (bind (closV-x handler) v)
                                          (closV-env handler)) (closV-e handler) k)]
             [plusLK (_ env k) (find-handler v k)]
             [plusRK (_ env k) (find-handler v k)]
             [timesLK (_ env k) (find-handler v k)]
             [timesRK (_ env k) (find-handler v k)]
             [appLK (_ env k) (find-handler v k)]
             [appRK (_ env k) (find-handler v k)]
             [tryLK (_ env k) (find-handler v k)]
             [raiseK (env k) (find-handler v k)]
             [haltK () (error 'find-handler "Uncaught raise.")])
  )

(define (apply-contin (k : Contin) (v : Value)) : Value
  (type-case Contin k
             [haltK () v]
             [plusLK (e2 env k)
                     (eval-env env e2 (plusRK v env k))]
             [plusRK (v1 env k)
                     (apply-contin k (numV (+ (numV-n v1) (numV-n v))))]
             [timesLK (e2 env k)
                     (eval-env env e2 (timesRK v env k))]
             [timesRK (v1 env k)
                     (apply-contin k (numV (* (numV-n v1) (numV-n v))))]
             [appLK (e2 env k)
                    (eval-env env e2 (appRK v env k))]
             [appRK (v1 env k)
                    (eval-env (extend-env (bind (closV-x v1) v) (closV-env v1)) (closV-e v1) k)]
             [tryLK (body env k)
                    (eval-env env body (tryRK v env k))]
             [tryRK (handler env k)
                    (apply-contin k v)]
             [raiseK (env k) (find-handler v k)]
             ;[else (error 'apply-contin "Not yet implemented")]
             ))

(define (eval-env (env : Env) (e : Expr) (k : Contin)) : Value
  (type-case Expr e
             [numC (n) (apply-contin k (numV n))]
             [plusC (e1 e2)
                    (eval-env env e1 (plusLK e2 env k))]
             [timesC (e1 e2)
                    (eval-env env e1 (timesLK e2 env k))]
             [lambdaC (x e) (apply-contin k (closV env x e))]
             [appC (e1 e2)
                   (eval-env env e1
                             (appLK e2 env k))]
             [letC (x e1 e2)
                   (eval-env env (appC (lambdaC x e2) e1) k)]
             [idC (x) (apply-contin k (lookup x env))]
             [raiseC (e)
                   (eval-env env e (raiseK env k))]
             [tryC (handler body)
                   (eval-env env handler (tryLK body env k))]
             )
  )

(define (eval (e : Expr))
  (eval-env empty-env e (haltK)))

(eval (parse '27))
(eval (parse '(+ 1 1)))
(eval (parse '(+ (+ 1 2) 3)))
(eval (parse '(* (+ 1 2) 3)))

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

(eval (parse '(try (lambda x (+ x 6)) 142)))
(eval (parse '(try (lambda x (+ x 6)) (raise 142))))
(eval (parse '(try (lambda x (+ x 6)) (raise (raise 5)))))

(eval (parse
       '(let f (try (lambda x (+ 10 x))
                    (lambda y (raise 1)))
          (try (lambda x (+ 20 x))
              (f 1)))))

; (parse '(try (lambda x (+ x 6)) (raise 1)))
; (parse '(try (lambda x (+ x 6)) (raise (+ 6 7))))
