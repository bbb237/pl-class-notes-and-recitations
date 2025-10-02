#lang plai-typed

;; Now let us play with Boolean expressions in typed Racket. 

;; 1: Define a type for Boolean expressions with booleans at the leaves.
;; Implement the function bexp-evaluate.
;; What benefits do the types confer? Try implementing bexp-evaluate-m that misses cases. 

(define-type bexp-b
  [leaf-node-b (b : symbol)]
  [and-node-b (b1 : bexp-b) (b2 : bexp-b)]
  [or-node-b (b1 : bexp-b) (b2 : bexp-b)]
  [not-node-b (b : bexp-b)])

(define (evaluate-bexp-b (b : bexp-b)) : boolean 
  (type-case bexp-b b
    [leaf-node-b (v)    v]
    [or-node-b (b1 b2)  (or (evaluate-bexp-b b1) (evaluate-bexp-b b2))]
    [and-node-b (b1 b2) (and (evaluate-bexp-b b1) (evaluate-bexp-b b2))]
    [not-node-b (b1)    (not (evaluate-bexp-b b1))]))

(evaluate-bexp-b (leaf-node-b #t))
; Expects: #t 
(evaluate-bexp-b (and-node-b (leaf-node-b #t) (leaf-node-b #f)))
; Expects #f
; (evaluate-bexp-b (and-node-b (leaf-node-b #t) (leaf-node-b 's)))
; Expects typechecking error 

#|
(define (evaluate-bexp-m s)
  (type-case bexp-b s
    [leaf-node-b (s) (s)]
 ))
|#

; Expects syntax error at compile time 

;; 2: Rewrite demorgan-opt in typed Racket, and annotate its input and output with types.

(define-type bexp
  [leaf-node (s : symbol)]
  [and-node (b1 : bexp) (b2 : bexp)]
  [or-node (b1 : bexp) (b2 : bexp)]
  [not-node (b1 : bexp)])

(define
  (demorgan-opt (b : bexp)) : bexp 
  (type-case bexp b
    ; Standard pattern matching
    [and-node (b1 b2) (and-node (demorgan-opt b1) (demorgan-opt b2))]
    [or-node (b2 b3) (or-node (demorgan-opt b2) (demorgan-opt b3))]
    [leaf-node (v) (leaf-node v)]
    [not-node (b1) (type-case bexp b1
                      ; DeMorgan pattern matching
                      [leaf-node (v) (not-node (leaf-node v))]
                      [and-node (b1 b2) (or-node (demorgan-opt (not-node b1)) (demorgan-opt (not-node b2)))]
                      [or-node (b1 b2) (and-node (demorgan-opt (not-node b1)) (demorgan-opt (not-node b2)))]
                      [not-node (b1) (demorgan-opt b1)])]))

; ~ (~ P \/ Q)
(define bexp-1
  (not-node
   (or-node
    (not-node (leaf-node 'P))
    (leaf-node 'Q))))
; Transforms to: P /\ ~ Q

; ~~ P \/ ~ Q
(define bexp-2
   (or-node
    (not-node (not-node (leaf-node 'P)))
    (not-node (leaf-node 'Q))))
; Transforms to: P \/ ~ Q

; P \/ ~ Q
(define bexp-3
   (or-node
    (leaf-node 'P)
    (not-node (leaf-node 'Q))))
; Transforms to: P \/ ~ Q

(demorgan-opt bexp-1)
(demorgan-opt bexp-2)
(demorgan-opt bexp-3)