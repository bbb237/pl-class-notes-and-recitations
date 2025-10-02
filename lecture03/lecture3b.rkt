#lang plai-typed

(define-type ArithE
  [numC (n : number)]
  [plusC (e1 : ArithE) (e2 : ArithE)]
  [timesC (e1 : ArithE) (e2 : ArithE)])


(define (parse (s : s-expression)) : ArithE
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let [(l (s-exp->list s))]
       (case (s-exp->symbol (first l))
         [(+) (plusC (parse (second l)) (parse (third l)))]
         [(*) (timesC (parse (second l)) (parse (third l)))]
         )
       )]
    ))


(define (eval (e : ArithE)) : number
  (type-case ArithE e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval e1) (eval e2))]
             [timesC (e1 e2) (* (eval e1) (eval e2))]))
