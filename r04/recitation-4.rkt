#lang plai-typed
(require (typed-in racket/base [gensym : (-> symbol)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 4: Pattern matching, continued                                                               ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let us continue to extend our running language with pattern matching.
;; In this recitation, we will look at variable pattern matching.
;; We will use this exercise to recap lazy vs. eager substitution. 

;; Recall our value-based and wildcard-based pattern matching expressions from last week.
; Example: match 5 with
;         | 5 -> 0
;         | 6 -> 1
; Expected: 0 

; Example: match (1 + 2) with        
;         | 3 -> 1
;         | _ -> x
; Expected: 1 

; Example: match x with        
;         | 0 -> z
;         | _ -> match y with
;               | 0 -> x
;               | 1 -> y
; Expected: unbound variable!

; Example: let x = 1 in
;            let y = 1 in
;              match x with        
;             | 0 -> z
;             | _ -> match y with
;                   | 0 -> x
;                   | 1 -> y + 1
; Expected: 2 


;; We extend our pattern matching expression with another case: (s, e)
;; where s is a symbol, and e is an expression.

;; First, let us look at some examples.
;; Try out the evaluation in (untyped) Racket!

;; Example 5: match x with y -> 10, _ -> 1
;; What should this evaluate to?

;; Example 6: let x = 10 in match x with 1 -> 2, 2 -> 3, y -> y + x, _ -> 4
;; What should this evaluate to?

;; Example 7: match 3 with y -> let y = 10 in y + y
;; What should this evaluate to?

;; 1. Type definitions
;; Let us define a type for patterns, and extend our ArithE type with them. 

(define-type PatternE
  [numP (n : number) (e : ArithE)]
  [anyP (e : ArithE)])

(define-type ArithE
  [numC (n : number)]
  [plusC (e1 : ArithE) (e2 : ArithE)]
  [timesC (e1 : ArithE) (e2 : ArithE)]
  [letC (x : symbol) (e1 : ArithE) (e2 : ArithE)]
  [idC (x : symbol)]
  [patternC (e : ArithE) (patterns : (listof PatternE))])

;; 2. Parsing 
;; Next, let's parse pattern matching expressions.
;; Hint: first define a helper function parse-patterns,
;; that takes in a list of s-expressions, and parses them to create a list of PatternE objects.
;; Then, extend our existing parse function to handle pattern matching.
(define (parse-patterns (s-pats : (listof s-expression))) : (listof PatternE)
    (cond
      [(empty? s-pats) empty]
      [else
       (let [(s-pat (s-exp->list (first s-pats)))]
         (cond
           [(s-exp-number? (first s-pat))
            (cons (numP (s-exp->number (first s-pat))
                        (parse (second s-pat)))
                  (parse-patterns (rest s-pats)))]
           [(equal? (s-exp->symbol (first s-pat)) '_) 
            (list (anyP (parse (second s-pat))))]))]))

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
         ; New case 
         [(match) (patternC (parse (second l)) (parse-patterns (rest (rest l))))]))]))

(define example-1-sexp '(match 5 (5 0) (6 1)))
(define example-2-sexp '(match (+ 1 2) (3 1) (_ x)))
(define example-3-sexp '(match x (0 z) (_ (match y (0 x) (1 y)))))
(define example-4-sexp '(let x 1 (let y 1 (match x (0 z) (_ (match y (0 x) (1 (+ 1 y))))))))

;(test (parse example-1-sexp) example-1)
;(test (parse example-2-sexp) example-2)
;(test (parse example-3-sexp) example-3)
;(test (parse example-4-sexp) example-4)

;; 3. Substitution
;; Here is where the design choice of lazy vs. eager substitution makes a difference

;; What is an example that separates these two approaches?

;; Approach 1: lazy substitution (notice the type signature)
(define (subst-pattern-lazy (en : ArithE) (x : symbol)) : (PatternE -> PatternE)
  (lambda (p) 
    (type-case PatternE p
      ; If our PatternE is a numP, we only need to substitute the expression 
      [numP (n e) (numP n (subst-lazy en x e))]
      ; If our PatternE is a anyP, we do not need to perform any substitution
      [anyP (e) (anyP (subst-lazy en x e))])))

; Use the above helper function to implement subst for patternC. Hint: use map!
(define (subst-lazy (erep : ArithE) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
    [idC (y) (if (equal? x y) erep (idC y))]
    [numC (n) (numC n)]
    [plusC (e1 e2) (plusC (subst-lazy erep x e1) (subst-lazy erep x e2))]
    [timesC (e1 e2) (timesC (subst-lazy erep x e1) (subst-lazy erep x e2))]
    [letC (y e1 e2)
          (cond
            [(equal? x y) (letC y (subst-lazy erep x e1) e2)]
            [else (let [(z (gensym))]
                    (letC z (subst-lazy erep x e1) (subst-lazy erep x (subst-lazy (idC z) y e2))))])]
    [patternC (e pats)
              (patternC (subst-lazy erep x e)
                        (map (subst-pattern-lazy erep x) pats))]))

;; Approach 2: eager substitution (notice the type signature)
(define (subst-pattern-eager (v : number) (x : symbol)) : (PatternE -> PatternE)
  (lambda (p) 
    (type-case PatternE p
      ; If our PatternE is a numP, we only need to substitute the expression 
      [numP (n e) (numP n (subst-eager v x e))]
      ; If our PatternE is a anyP, we do not need to perform any substitution
      [anyP (e) (anyP (subst-eager v x e))])))

(define (subst-eager (v : number) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
    [idC (y) (if (equal? x y) (numC v) e)]
    [numC (n) (numC n)]
    [plusC (e1 e2) (plusC (subst-eager v x e1) (subst-eager v x e2))]
    [timesC (e1 e2) (timesC (subst-eager v x e1) (subst-eager v x e2))]
    [letC (y e1 e2) (letC y (subst-eager v x e1) (subst-eager v x e2))]
    [patternC (e pats)
              (patternC (subst-eager v x e)
                        (map (subst-pattern-eager v x) pats))]))

;; 4. Evaluation 
;; Finally, let's extend the eval function to evaluate pattern-matching.
;; Again, let's define a helper function get-pat-expr that given a pattern:
;; (match e (n1 e1) (n2 e2) .... (x en))
;; represented as a list of PatternE objects, and num which is the evaluation result of e,
;; returns the matching ei.
;; Pay special attention to the symP case: what else needs to be done here? 

(define (get-pat-expr (v : number) (pats : (listof PatternE))) : ArithE
  (cond
    [(empty? pats) (error 'get-pat-expr "unmatched pattern")]
    [else (type-case PatternE (first pats)
            ; Value pattern only matches if the values are equal
            [numP (n e) (cond
                          [(equal? n v) e]
                          [else (get-pat-expr v (rest pats))])]
            ; Wildcard pattern matches everything 
            [anyP (e) e])]))

; Use get-pat-expr to implement eval for patternC.

(define (eval (e : ArithE)) : number
  (type-case ArithE e
    [numC (n) n]
    [plusC (e1 e2) (+ (eval e1) (eval e2))]
    [timesC (e1 e2) (* (eval e1) (eval e2))]
    [letC (x e1 e2)
          (eval (subst-lazy e1 x e2))]
    [patternC (e1 pats) (eval (get-pat-expr (eval e1) pats))]
    [idC (x) (error 'eval "unbound variable!")]))

(define (eval-eager (e : ArithE)) : number
  (type-case ArithE e
    [numC (n) n]
    [plusC (e1 e2) (+ (eval e1) (eval e2))]
    [timesC (e1 e2) (* (eval e1) (eval e2))]
    [letC (x e1 e2)
          (local ([define res (eval-eager e1)])
            (eval (subst-eager res x e2)))]
    [patternC (e1 pats) (eval (get-pat-expr (eval e1) pats))]
    [idC (x) (error 'eval "unbound variable!")]))

;; Examples from last week:
(define example-1 : ArithE
  (patternC
   (numC 5)
   (list
    (numP 5 (numC 0))
    (numP 6 (numC 1)))))

(define example-2 : ArithE
  (patternC
   (plusC (numC 1) (numC 2))
   (list
    (numP 3 (numC 1))
    (anyP (idC 'x)))))

(define example-3 : ArithE
  (patternC
   (idC 'x)
   (list
     (numP 0 (idC 'z))
     (anyP (patternC
            (idC 'y)
            (list
             (numP 0 (idC 'x))
             (numP 1 (idC 'y))))))))

(define example-4 : ArithE
  (letC
   'x
   (numC 1)
   (letC
    'y
    (numC 1)
    (patternC
     (idC 'x)
     (list
      (numP 0 (idC 'z))
      (anyP (patternC
             (idC 'y)
             (list
              (numP 0 (idC 'x))
              (numP 1 (plusC (numC 1) (idC 'y)))))))))))