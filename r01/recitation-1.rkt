#lang racket
(require test-engine/racket-tests)
(define (all l) (andmap identity l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                                          ;;
;;  Recitation 1: Functional Programming in Racket                                                          ;;
;;                                                                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; This recitation will cover basic functional programming in Racket, and working in the Dr.Racket IDE.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Expressions and Functions

; Racket is an expression-oriented langauge: Racket programs are essentially lists of
; expressions. Every Racket program evaluates by reducing itself to a value.

(define (is-even x)
  (= 0 (modulo x 2)))


;; What will the following expressions evaluate to?
;; Try them in the DrRacket REPL.
; > is-even
; > (is-even)
; > (is-even 2)
; > (is-even 3)
; > (is-even #t)
; > (is-even 'four)





; In the functional programming style, we build programs by function composition.
; In this exercise, we will practice this form of composition.


;; A traffic light is one of
;; 'red
;; 'yellow
;; 'green

;; The function can-go
;; EXPECTS a traffic light
;; PRODUCES a boolean
;; (can-go l) evaluates to #t when a vehicle approaching a light can safely enter the intersection.

;; What is the difference between eq? and equal?

(define (can-go l) (equal? l 'green))

(check-expect (can-go 'green) #t)
(check-expect (can-go 'yellow) #f)
(check-expect (can-go 'red) #f)

;; The function next-light
;; EXPECTS a traffic light
;; PRODUCES a traffic light
;; (next-light l) evaluates to the colour of the light after l
(define (next-light l)
  (match l
    ['red    'green]
    ['yellow 'red]
    ['green  'yellow]))


(check-expect (next-light 'green) 'yellow)
(check-expect (next-light 'yellow) 'red)
(check-expect (next-light 'red) 'green)


;;
;; Exercise #1: Fill in the following function:
;;

;; The function can-floor-it
;; EXPECTS a traffic light
;; PRODUCES a boolean
;; (can-floor-it l) evaluates to #t when a vehicle can safely enter the intersection at
;; either the current light, or the next light.
(define (can-floor-it l) 'TODO)



;; Once you're done, practice writing check-expect tests for can-floor-it, and evaluating
;; can-floor-it in the Dr.Racket REPL.



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Recursive Functions
;;


; Recursive functions are a common idiom in functional programming. In this exercise,
; we will walk through the process for implementing a recursive function.

;;
;; Exercise #2.1: Given the following specification, answer the following questions
;;


;; The function next-light-iter
;; EXPECTS: a non-negative integer n
;; EXPECTS: a traffic light l
;; PRODUCES: a traffic light
;; (next-light-iter n l) gives the colour of the traffic light l, after waiting for it
;; to change n times.


; - When n is zero, what do we expect (next-light-iter n l) to evaluate to?
; 


; - When n is greater than zero, what do we expect (next-light-iter n l) to evaluate to?
;


;;
;; Exercise #2.2: Using the answers to the questions above, fill in this partially completed
;; implementation of next-light-iter.
;;

(define (next-light-iter n l)
  (if (eq? n 0)
      ;; Case when n = 0
      'TODO

      ;; Case when n > 0
      'TODO))


;;
;; Exercise #2.3: Using the answers to the questions above, write tests for next-light-iter.
;;




;;
;; Exercise #3: Using the same process, fill in the following recursive function.
;;



;; A vehicle is one of
;; 'sportscar
;; 'suv
;; 'prius


;; The function speed
;; EXPECTS a vehicle
;; PRODUCES an integer
;; (speed v) gives the speed of the vehicle in meters per second.
(define (speed v)
  (match v
    ['sportscar 136]
    ['suv       48]
    ['prius     27]))


;; The function time-to-change
;; EXPECTS a traffic light l
;; RETURNS a non-negative integer
;; (time-to-change l) gives the number of seconds a traffic light
(define (time-to-change l)
  (match l
    ['red       10]
    ['yellow    2]
    ['green     23]))


;; The function safe-to-scroll
;; EXPECTS: a vehicle v
;; EXPECTS: a traffic light l
;; EXPECTS: a non-negative integer d, representing the distance from the vehicle to the light
;; RETURNS: a boolean value
;; (safe-to-scroll v d l) returns #t when
;;  if v is d meters away from an intersection which has just changed to the light l,
;;  v will enter the intersection without incurring a traffic violation.


; What do we expect (safe-to-scroll v l d) to be when the vehicle will enter the
; intersection before it switches?
; 

; What do we expect (safe-to-scroll v l d) to be when the vehicle will not enter
; the intersection before it switches?
; 


; HINT: Consider defining a helper function to determine if the vehicle will enter the
; intersection before the light switches.

;; Determine if the vehicle v at distance d from the intersection will enter the intersection before the light changes

#;
(check-expect (safe-to-scroll 'TODO 'TODO 'TODO) 'TODO)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lists and higher-order programming
;;

; Recall the function is-even

#;
(define (is-even x)
  (= 0 (modulo x 2)))

; In this section, we will practice functional programming by writing a program to check
; if every element in a list is even, using this function.


;; Warmup: List operations cons and append
; What do we expect the following to return?
; (cons 3 4)
; (cons 3 '(4))
; (cons '(4) 3)
; (append 3 4)
; (append 3 '(4))
; (append '(3) 4) 

;;
;; Exercise #4: First, we can write a recursive function just like we did above.
;;

;; The function all-even-rec
;; EXPECTS: a list of integers l
;; PRODUCES: a boolean
;; (all-even-rec l) is #t when all numbers in the list are even
(define (all-even-rec l)
  (match l
    ['()              'TODO]
    [(cons head tail) 'TODO]))
















;; Exercise #5: In class you learned about the map function, which lets you apply a function to every
;; element of a list.
;;
;; Racket comes with built-in ways to avoid writing our own recursive functions all the
;; time. At the top of this file, we included a function (all ...) which checks to see
;; if a list of booleans are all #t:

(check-expect (all (list)) #t)
(check-expect (all (list #t #t #t)) #t)
(check-expect (all (list #t #f #t)) #f)

;; Using map and all, write a function to check if all elements of a list are even.


(define (all-even-map l) 'TODO)













;; Exercise #6: Write a function to reverse a list.

(define (reverse-list l)
  (match l
    ['() 'TODO]
    [(cons head tail) 'TODO]))








;; Exercise #7: Tail recursion

; Imagine a list with millions of elements. In that case, a function that goes over every element
; of the list would create a huge call stack.

; Example, say we run (reverse-list '(1 2 3 4 5 6 7)).
; The call would reduce to
;     (append (reverse-list '(2 3 4 5 6 7)) (list 1))
;     (append (append (reverse-list '(3 4 5 6 7)) (list 2)) (list 1))
;     (append (append (append (reverse-list '(4 5 6 7)) (list 3)) (list 2)) (list 1))
;     ....

; Languages like Racket, which encourage functional programming, support tail-call optimization.
; In case the last operation of a function is just another function call, Racket pops the current
; function from the call stack.

; Write your own version of (reverse l) that can leverage tail-call optimization.

; Hint: Use a helper function. reverse can only have one argument. On the other hand, you can use
; the helper function to have multiple arguments that might store some more information.

;; Write your solution here









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test)
