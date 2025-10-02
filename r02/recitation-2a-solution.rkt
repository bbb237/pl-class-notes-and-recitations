#lang racket
(require test-engine/racket-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 2: Boolean expressions                                                                       ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let's consider a language bexp of boolean expressions, defined as follows: 
;;
;;  bexp := leaf-node l
;;        | or-node bexp bexp
;;        | and-node bexp bexp
;;        | not-node bexp

;; In (untyped) racket, we can define this langauge without committing to a leaf type: 

(struct leaf-node (arg))
(struct or-node (arg1 arg2))
(struct and-node (arg1 arg2))
(struct not-node (arg))


;; For example, leaf nodes could contain boolean values:

(define bexp-bool-1 (and-node (leaf-node #t) (leaf-node #f)))

;; Alternatavely, leaf nodes could contain symbols: 

(define bexp-symbol-1 (and-node (leaf-node 'x) (leaf-node 'y)))

;; Untyped racket accepts both of these definitions, we will revisit this later
;; in the recitation.




;; 1. A bexp boolean interpreter
;;
;; First, let's assume the leaves store boolean values (#t and #f in Racket).
;; Define an interpreter for the bexp langauge, which calculates the boolean
;; value represented by the expression.
(define (evaluate-bexp b)
  (match b
    [(leaf-node v)    v]
    [(or-node b1 b2)  (or (evaluate-bexp b1) (evaluate-bexp b2))]
    [(and-node b1 b2) (and (evaluate-bexp b1) (evaluate-bexp b2))]
    [(not-node b1)    (not (evaluate-bexp b1))]))


;; Write your tests here:
(check-expect (evaluate-bexp (leaf-node #t)) #t)
(check-expect (evaluate-bexp (and-node (leaf-node #t) (leaf-node #f))) #f)
;; ... 


;; Q: What happens when you miss cases in the pattern match? 
(define (evaluate-bexp-m b)
  (match b
    [(leaf-node v)    v]
    [(or-node b1 b2)  (or (evaluate-bexp-m b1) (evaluate-bexp-m b2))]
    [(and-node b1 b2) (and (evaluate-bexp-m b1) (evaluate-bexp-m b2))]))



;; 2. A bexp compiler optimization pass 
;; 
;; Let's go back to the general version of bexp, where leaf nodes store symbols. 
;;
;; In propositional logic, De Morgan's laws state the following equivalences: 
;;
;;  not (P and Q)  =  (not P) or  (not Q)
;;  not (P or  Q)  =  (not P) and (not Q)
;;
;; Double negation elimination states the following equivalence:
;;
;;  not (not P)    =  P

;; One possible way to optimize a bexp program is to use the above equivalences to
;; eliminate redundant negations, and to push all negations down to the leaves.
;; Such an optimization is useful for e.g. SAT solving using the DPLL algorithm. 

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

;; Design a function to perform this optimization pass.
;;
;; Carefully consider the pattern match cases: you will need more cases than we used in evaluate-bexp

(define
  (demorgan-opt b)
  (match b
    ; Standard pattern matching
    [(and-node b1 b2) (and-node (demorgan-opt b1) (demorgan-opt b2))]
    [(or-node b2 b3) (or-node (demorgan-opt b2) (demorgan-opt b3))]
    [(leaf-node v) (leaf-node v)]
    [(not-node (leaf-node l)) (not-node (leaf-node l))]
    ; DeMorgan pattern matching
    [(not-node (and-node b1 b2)) (or-node (demorgan-opt (not-node b1)) (demorgan-opt (not-node b2)))]
    [(not-node (or-node b1 b2)) (and-node (demorgan-opt (not-node b1)) (demorgan-opt (not-node b2)))]
    [(not-node (not-node b1)) (demorgan-opt b1)]))
    

;; A cursory test function: determine if two bexp are syntactically equal
(define
  (syn-equal? b1 b2)
  (match* (b1 b2)
    [((leaf-node v1) (leaf-node v2)) (equal? v1 v2)]
    [((and-node b1a b1b) (and-node b2a b2b)) (and (syn-equal? b1a b2a) (syn-equal? b1b b2b))]
    [((or-node b1a b1b) (or-node b2a b2b)) (and (syn-equal? b1a b2a) (syn-equal? b1b b2b))]
    [((not-node b3) (not-node b4)) (syn-equal? b3 b4)]
    [(_ _) #f]))

; P /\ ~ Q
(define bexp-1-opt
   (and-node
    (leaf-node 'P)
    (not-node (leaf-node 'Q))))

; P \/ ~ Q
(define bexp-2-opt 
   (or-node
    (leaf-node 'P)
    (not-node (leaf-node 'Q))))

; P \/ ~ Q
(define bexp-3-opt 
   (or-node
    (leaf-node 'P)
    (not-node (leaf-node 'Q))))

(check-expect (syn-equal? (demorgan-opt bexp-1) bexp-1-opt) #t)
(check-expect (syn-equal? (demorgan-opt bexp-2) bexp-2-opt) #t)
(check-expect (syn-equal? (demorgan-opt bexp-3) bexp-3-opt) #t)


;; A better test function: determine if a bexp expression only has not nodes at the leaves.
(define
  (only-not-at-leaves? b)
  (match b
    [(leaf-node v) #t]
    [(or-node b1 b2) (and (only-not-at-leaves? b1) (only-not-at-leaves? b2))]
    [(and-node b1 b2) (and (only-not-at-leaves? b1) (only-not-at-leaves? b2))]
    [(not-node (leaf-node v)) #t]
    [(not-node _) #f]))

;; Overall test function: 
(define
  (demorgan-opt-correct? b)
  (and
   ;; Optimization pass preserves the semantics of bexp
   (equal? (evaluate-bexp b) (evaluate-bexp (demorgan-opt b)))
   ;; Optimization pass produces trees with not nodes only occurring at leaves 
   (only-not-at-leaves? (demorgan-opt b))))


;; Test your optimization pass using the demorgan-opt-correct? function. 
(check-expect (demorgan-opt-correct? (leaf-node #t)) #t)

(define bexp-4
  (not-node
   (or-node
    (not-node (leaf-node #t))
    (leaf-node #f))))
(define bexp-5
   (or-node
    (not-node (not-node (leaf-node #t)))
    (not-node (leaf-node #f))))
(define bexp-6
   (or-node
    (leaf-node #t)
    (not-node (leaf-node #t))))
(check-expect (demorgan-opt-correct? bexp-4) #t)
(check-expect (demorgan-opt-correct? bexp-5) #t)
(check-expect (demorgan-opt-correct? bexp-6) #t)


(test)