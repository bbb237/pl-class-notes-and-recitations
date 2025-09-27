#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 3: Pattern matching                                                                         ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pattern matching can be understood as a more flexible, concise alternative to conditionals. 
;; A simple example with two patterns, expressible using a single if-then-else: 

(define (factorial x) 
  (match x 
    [0 1]
    [else (* x (factorial (- x 1)))]))

(define (factorial2 x)
  (if (= x 0)
      1
      (* x (factorial2 (- x 1)))))

(factorial 1)
(factorial 10)
(factorial 25)

;; Concision gain increases with the number of cases: 
(define (fibonacci x)
  (match x 
    [0 0]
    [1 1]
    [else (+ (fibonacci (- x 1)) (fibonacci (- x 2)))]))

(define (fibonacci2 x)
  (if (= x 0)
      0
      (if (= x 1)
          1
          (+ (fibonacci (- x 1)) (fibonacci (- x 2))))))

(fibonacci 1)
(fibonacci 10)
(fibonacci 25)

;; In addition to matching on values, matching on algebraic datatypes provides significant benefits. 

;; Recall our suite of functions for manipulating Boolean expressions from the last recitation:
;; - evaluate-bexp uses pattern matching to define evaluation rules for each operator node,
;; - demorgan-opt uses pattern matching to apply transformation rules to bexps of a certain shape,
;; - syn-equal? uses pattern matching to compare two bexps in parallel,
;; - only-not-at-leaves? uses pattern matching to check where not nodes specifically appear in a bexp.

;; Imagine if we had to rewrite our Boolean expression functions using only conditionals!

;; Finally, pattern matching can be used to destructure structured data,
;; such as the third element in a list:
(define (my-third x)
  (match x
    [(list a b c d) c]
    [else x]))

(define (other-third lst)
  (match lst
    [(cons _ (cons _ (cons third _))) third]
    [_ (error "List too short")]))

(my-third (list 2 3))
(my-third 0)
(my-third (list '(2 3) 2 4 5 6 7))
(my-third (list 1 2 3 4))

(other-third (list 2 3))
(other-third 0)
(other-third (list '(2 3) 2 4 5 6 7))
(other-third (list 1 2 3 4))