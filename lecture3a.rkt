#lang plai-typed

;; Things to run

3

(+ 3 4)

"hello"

; (+ 3 "hello")

(define (add3 x)
  (+ 3 x))

(add3 4)


(define (add3-v2 (x : number))
  (+ 3 x))

(define (add3-v3 (x : number)) : number
  (+ 3 x))

; (define (add3-v4 (x : number)) : string
;   (+ 3 x))

add3

(define (myadd x y)
  (+ x y))

(define (myadd-v2 (x : number) y) : number
  (+ x y))

(define (myadd-v3 (x : number))
  (lambda (y) (+ x y)))

(list 1 2 3 4)

(cons 3 (cons 4 empty))

; (cons 3 4)

(pair 1 2)

; car (pair 1 2)
; car (list 1 2 3 4)

(fst (pair 1 2))
(snd (pair 1 2))

(first (list 1 2 3 4))
(second (list 1 2 3 4))
(third (list 1 2 3 4))
(rest (list 1 2 3 4))

first
rest
cons
fst
snd

map

(define-type Shape
  [circle (radius : number)]
  [rectangle (width : number) (height : number)])
       
(define-type Tree
  [emp]
  [node (n : number) (lt : Tree) (rt : Tree)])

(define (area s)
  (type-case Shape s
             [circle (r) (* 3.14159 (* r r))]
             [rectangle (w h) (* w h)]))

(define (area-v2 s)
  (cond
    [(circle? s) (* 3.14159 (* (circle-radius s) (circle-radius s)))]
    [(rectangle? s) (* (rectangle-width s) (rectangle-height s))]))
             
              
; Catches that we forgot the rectangle case
; (define (area-v3 s)
;   (type-case Shape s
;              [circle (r) (* 3.14159 (* r r))]))

; No static error. 
(define (area-v4 s)
  (cond
    [(circle? s) (* 3.14159 (* (circle-radius s) (circle-radius s)))]))

'x

'(+ 2 3)

(s-exp->list '(+ 2 3))

(first (s-exp->list '(+ 2 3)))
(second (s-exp->list '(+ 2 3)))

(s-exp->symbol (first (s-exp->list '(+ 2 3))))
(s-exp->number (second (s-exp->list '(+ 2 3))))
