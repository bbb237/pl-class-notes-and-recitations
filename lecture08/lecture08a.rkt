#lang plai-typed


;;;;;;;;;;;;

(define-type Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [listV (vs : (listof Value))])

(define-type Expr
  [valE (v : Value)]
  [plusE (e1 : Expr) (e2 : Expr)]
  [equal?E (e1 : Expr) (e2 : Expr)]
  [ifE (guard : Expr) (e1 : Expr) (e2 : Expr)]
  [listE (es : (listof Expr))]
  [consE (e1 : Expr) (e2 : Expr)]
  [firstE (e : Expr)]
  [restE (e : Expr)]
  [unpackE (vars : (listof symbol)) (e1 : Expr) (e2 : Expr)]
  [letE (bindings : (listof (symbol * Expr))) (e : Expr)]
  [let*E (bindings : (listof (symbol * Expr))) (e : Expr)]
  [lambdaE (params : (listof symbol)) (e : Expr)]
  [appE (es : (listof Expr))]
  )

(define-type CoreValue
  [numCV (n : number)]
  [boolCV (b : boolean)]
  [pairCV (v1 : Value) (v2 : Value)])

(define-type CoreExpr
  [valC (v : CoreValue)]
  [plusC (e1 : CoreExpr) (e2 : CoreExpr)]
  [pairC (e1 : CoreExpr) (e2 : CoreExpr)]
  [fstC (e : CoreExpr)]
  [sndC (e : CoreExpr)]
  [letC (x : symbol) (e1 : CoreExpr) (e2 : CoreExpr)]
  [lambdaC (x : symbol) (e : CoreExpr)]
  [appC (e1 : CoreExpr) (e2 : CoreExpr)]
  [idC (x : symbol)]
  [boxC (e : CoreExpr)]
  [unboxC (e : CoreExpr)]
  [setboxC (e1 : CoreExpr) (e2 : CoreExpr)]
  [seqC (e1 : CoreExpr) (e2 : CoreExpr)]
  )

(define (eval-core (ce : CoreExpr)) : CoreValue
  (error 'eval-core "Not implemented."))

; "Elaboration"
(define (expr->core-expr (e : Expr)) : CoreExpr
  (error 'expr->core-expr "Not implemented."))

(define (core-value->value (cv : CoreValue)) : Value
  (error 'core-value->value "Not implemented."))

(define (eval (e : Expr)) : Value
  (core-value->value (eval-core (expr->core-expr e))))


; Parse -> Elaborate -> Evaluate
