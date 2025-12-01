#lang plai-typed

(define (f1 x)
  x)

(define f2
  (lambda (x) x))

(define (f3 x)
  (begin (+ 1 2)
         x))

(define b (box 0))

(define f4
  (lambda (x)
    (begin (set-box! b 1)
           x)))

(define f5
  (lambda (x)
    (first (cons x (cons x empty)))))

(define f6
  (lambda (x)
    (begin (error 'f6 "Hello")
           x)))

(define (f7 x)
  (begin (f7 x)
         x))

; If a function f has type ('a -> 'a), and (1) f is *pure*,
; meaning no side-effects, (2) f never raises errors, and (3) always terminates
; then f is equivalent to the identity function
;

(define (h1 x)
  (fst x))

; g : (('a * 'b) -> 'b)

(define (g1 x)
  (snd x))

; q : (('a * 'a) -> 'a)

; (1) q always returns the first component
; (2) q always returns the second component

(define (q1 (p : ('a * 'a))) : 'a
  (fst p))

(define (q2 (p : ('a * 'a))) : 'a
  (snd p))

(define (q3 (p : ('a * 'a))) : 'a
  (if (equal? (fst p) (snd p))
      (fst p)
      (snd p)))

; (listof 'a) -> (listof 'a)

#| The function f has type (listof 'a) -> (listof 'a) and f is pure, terminating, etc. etc.
; suppose I run (f (list 1 2 3 4)) is it possible that the output list contains 11?
; |#

#| The function f has type (listof number) -> (listof number) and f is pure ....
; suppose I run (f (list 1 2 3 4), is it possible that the output list contains 11?
|#



(define (s1 (l : (listof 'a))) : (listof 'a)
  (rest l))

(define (s2 (l : (listof 'a))) : (listof 'a)
   (map (lambda (x) x) l))

(define (s3 (l : (listof 'a))) : (listof 'a)
  l)

(define (s4 l)
  (append l l))

(define (s5 (l : (listof 'a))) : (listof 'a)
  empty)

; ('a ('a -> 'b) -> 'b)

(define (c1 (x : 'a) (f : ('a -> 'b))) : 'b
  (f x))

(define (c2 (x : 'a) (f : ('a -> 'b))) : 'b
  (begin (f x)
         (f x)))

; ('a ('a -> 'a) -> 'a)

(define (r1 (x : 'a) (f : ('a -> 'a))) : 'a
  x)


(define (r2 (x : 'a) (f : ('a -> 'a))) : 'a
  (f x))


(define (r3 (x : 'a) (f : ('a -> 'a))) : 'a
  (f (f x)))


(define (r4 (x : 'a) (f : ('a -> 'a))) : 'a
  (f (f (f x))))

; (('a -> 'b) (listof 'a) -> (listof 'b))

(define (d1 (f : ('a -> 'b)) (l : (listof 'a))) : (listof 'b)
  (if (empty? l)
      empty
      (list (f (first l)))))

; (('a -> 'a) (listof 'a) -> (listof 'a))

; ('a 'a boolean -> 'a)

(define (y1 (x1 : 'a) (x2 : 'a) (b : boolean)) : 'a
  (if b
      x1
      x2))


(define (y2 (x1 : 'a) (x2 : 'a) (b : boolean)) : 'a
  x1)


(define (y3 (x1 : 'a) (x2 : 'a) (b : boolean)) : 'a
  x2)

; ('a ('a -> 'a) number -> 'a)
