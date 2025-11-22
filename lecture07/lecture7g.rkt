#lang racket

 ;; Lecture07 notes on object-oriented programming with delegation

(define o
  (lambda (m)
    (case m
    [(add) (lambda (x y) (+ x y) )]
    [(addl) (lambda (x) (+ x 1))]
    [ (subl) (lambda (x) (- x 1))])))

(define (msg o m . a)
  (apply (o m) a))

(define (o-constr x)
  (lambda (m)
    (case m
      ( (addx) (lambda (y) (+ x y))))))

(define (obj-counter count)
  (let ([b (box count) ])
    (lambda (m)
    (case m
      [ (inc) (lambda () (set-box! b (+ 1 (unbox b))) )]
      [ (dec) (lambda () (set-box! b (- (unbox b) 1)))]
      [ (get) (lambda () (unbox b))]))))

;; Shared mutable state across different versions of this object
(define o-static-1
  (let ( [numobj (box 0)])
    (lambda (amount)
      (begin
        (set-box! numobj (+ 1 (unbox numobj)))
        (lambda (m)
          (case m
            [(addX) (lambda (n) (+ amount n))]
            [(count) (lambda () (unbox numobj))]))))))

;; Added a box local/private to a particular version of the object.
;; We are able to mix and match fields that are shared across
;; objects and ones that are private to a particular object.
(define o-static-2
  (let ([numobj (box 0)])
    (lambda (amount)
      (let ([b (box amount)])
        (begin
          (set-box! numobj (+ 1 (unbox numobj)))
          (lambda (m)
            (case m
              [(addx) (lambda (n) (set-box! b (+ n (unbox b))))]
              [(get) (lambda () (unbox b))]
              [(count) (lambda () (unbox numobj))])))))))

;; Its common in OO languages that when you have a method, you can refer to the
;; object on which that method is being invoked by using a variable called either
;; 'self' or 'this'. In the body of the method, 'self' or 'this' will refer to
;; the object on which the method is being invoked on. 
;; We can encode that kind of recursion by using mutable state. One version of
;; this uses boxes to create that recursion. Implicit self variable using mutable boxes
(define o-self!
  (let ([self (box 0)])
    (begin
      (set-box! self
                (lambda (m)
                  (case m
                    [ (first) (lambda (x) (msg (unbox self) 'second (+ x 1)))]
                    [(second) (lambda (x) (+ x 1))])))
      (unbox self))))

;; every method takes an additional argument which is that recursive reference
;; itself. It takes a first argument called 'self' or 'this'. Implemented using
;; lambda. Very error prone to have the user pass in the recursive reference
;; to the object. 
(define o-self
  (lambda (m)
    (case m
      [(first) (lambda (self x) (msg self 'second self (+ x 1)))]
      [(second) (lambda (self x) (+ x 1))])))

;; We can implement the automatic passing by having a better version of our
;; msg function. What it does is it applies o to m, so it applies object
;; to the method name, then it passes the object itself again and then
;; passes the rest of the arguments.
(define o-self1
  (lambda (m)
    (case m
      [(first) (lambda (self x) (msg/self 'second (+ x 1)))]
      [(second) (lambda (self x) (+ x 1))])))

(define (msg/self o m . a)
  (apply (o m) o a))