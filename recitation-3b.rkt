#lang plai-typed
(require (typed-in racket/base [gensym : (-> symbol)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 3: Pattern matching                                                                         ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let us extend our running language with pattern matching.
;; For now, we will only support value-based (number-based) pattern matching,
;; a la our factorial and fibonacci functions. 
;; Thus, pattern matching expressions will look like:
;; (match e (n1 e1) (n2 e2) .... (x en))
;; where n1, n2 are numbers, and x is a symbol.

;; The semantics evaluates e and compares it against n1, n2 ... in turn.
;; If e is equal to ni for some i, then the entire match expression evaluates to ei.
;; If ni = nj for some i < j, then the match expression evaluates to ei.
;; If e is not equal to ni for any i, then if there is a symbol case, it evaluates
;; to en, otherwise it throws an error.

;; 1. Type definitions
;; Let us define a type for patterns, and extend our ArithE type with them. 

(define-type PatternE
  [numP (n : number) (e : ArithE)]
  [symP (s : symbol) (e : ArithE)])

(define-type ArithE
  [numC (n : number)]
  [plusC (e1 : ArithE) (e2 : ArithE)]
  [timesC (e1 : ArithE) (e2 : ArithE)]
  [letC (x : symbol) (e1 : ArithE) (e2 : ArithE)]
  [idC (x : symbol)]
  [patternC (e : ArithE) (patterns : (listof PatternE))])

; Example: match 5 with
;         | 5 -> 0
;         | 5 -> 1 
(define example-1 : ArithE
  (patternC
     (numC 5)
     (list
      (numP 5 (numC 0))
      (numP 6 (numC 1)))))

; Example: match (1 + 2) with        
;         | 3 -> 1
;         | x -> x
(define example-2 : ArithE
  (patternC
     (plusC (numC 1) (numC 2))
     (list
      (symP 'x (idC 'x))
      (numP 3 (numC 1)))))

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
           [(s-exp-symbol? (first s-pat))
            (cons (symP (s-exp->symbol (first s-pat))
                        (parse (second s-pat)))
                  (parse-patterns (rest s-pats)))]))]))

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

(test (parse-patterns (list)) (list))
(test (parse-patterns (list `(4 4))) (list (numP 4 (numC 4))))
(test (parse-patterns (list `(x 4))) (list (symP 'x (numC 4)))) 
(test (parse-patterns (list '(4 4) `(x (+ 3 4)))) (list (numP 4 (numC 4)) (symP 'x (plusC (numC 3) (numC 4)))))


;; 3. Substitution 
;; If we match on a symbol in a pattern matching expression match e (x en),
;; We want to return en[e/x], meaning that we replace all free occurrences of x in en with e.
;; For example, suppose we want to substitute the expression (+ 3 2) for y in the expression 
;; (y (+ y 2))
;; We should end up with:
;; ((+ 3 2) (+ (+ 3 2) 2))
;; Except this is not a well-formed PatternE object, because the first element must be a single symbol
;; Which we know evaluates to five 
;; (z' (+ z' 2))
;; We define a helper function subst-pattern first which substitutes free occurrences of x
;; with erep for individual PatternE objects.

#;
(define (subst-pattern (erep : ArithE) (x : symbol) (p : PatternE)) : PatternE
    (type-case PatternE pat
      [numP (n e) 'TODO]
      [anyP (s e) 'TODO]
      )
    )

; Use the subst-pattern function to implement subst for patternC. Hint: use map!
#;
(define (subst (erep : ArithE) (x : symbol) (e : ArithE)) : ArithE
  (type-case ArithE e
    [idC (y) (if (equal? x y) erep (idC y))]
    [numC (n) (numC n)]
    [plusC (e1 e2) (plusC (subst erep x e1) (subst erep x e2))]
    [timesC (e1 e2) (timesC (subst erep x e1) (subst erep x e2))]
    [letC (y e1 e2)
          (cond
            [(equal? x y) (letC y (subst erep x e1) e2)]
            [else 
             (let [(z (gensym))]
               (letC z (subst erep x e1) (subst erep x (subst (idC z) y e2))))])]))

;; 4. Evaluation 
;; Finally, let's extend the eval function to evaluate pattern-matching.
;; Again, let's define a helper function get-pat-expr that given a pattern:
;; (match e (n1 e1) (n2 e2) .... (x en))
;; represented as a list of PatternE objects, and num which is the evaluation result of e,
;; returns the matching ei.
;; Pay special attention to the symP case: what else needs to be done here? 

#;
(define (get-pat-expr (num : number) (pats : (listof PatternE))) : ArithE
  'TODO
  )

;; Use get-pat-expr to implement eval for patternC.

#;
(define (eval (e : ArithE)) : number
  (type-case ArithE e
             [numC (n) n]
             [plusC (e1 e2) (+ (eval e1) (eval e2))]
             [timesC (e1 e2) (* (eval e1) (eval e2))]
             [letC (x e1 e2)
                     (eval (subst e1 x e2))
                   ]
             [idC (x) (error 'eval "unbound variable!")]
             )
  )

(eval (parse '(match 3 (3 4) (4 5) (x y))))
(eval (parse '(match 3 (1 4) (2 15) (3 17))))
(eval (parse '(let n 0 (match n (0 0) (1 1) (n (+ n 100))))))
(eval (parse '(let n (+ 1 0) (match n (0 0) (1 1) (m (+ 1 100))))))