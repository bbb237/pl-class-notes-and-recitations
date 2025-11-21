#lang racket

(define (fact n)
  (if (equal? n 0)
      1
      (* n (fact (- n 1)))))

; (* 5 (fact 4)
; (* 5 (* 4 (fact 3))
; (* 5 (* 4 (* 3 (fact 2)))
; (* 5 (* 4 (* 3 (* 2 (fact 1))
; (* 5 (* 4 (* 3 (* 2 (* 1 (fact 0))
; .... * 1)

(define (fact-acc n acc)
  (if (equal? n 0)
      acc
      (fact-acc (- n 1) (* n acc))))

(define (fact2 n)
  (fact-acc n 1))

(define (fact-contin n k)
  (if (equal? n 0)
      (k 1)
      (fact-contin (- n 1) (lambda (x) (k (* n x))))))

(define (fact3 n)
  (fact-contin n (lambda (x) x)))

; (fact-contin 1 k)
; --> (fact-contin 0 (lambda (x) (* 1 x)))
; --> ((lambda (x) (* 1 x)) 1)
; --> 1

; [a; b; c; d; e]

; append [e; d; c; b] [a]

;  [e; d; c; b; a]

; l = [a]
; (append (rev empty) (list a))
; (append empty (list a)
; [a]

(define (rev l)
  (if (equal? l empty)
      empty
      (append (rev (rest l)) (list (first l)))))

(define (rev-acc l acc)
  (if (equal? l empty)
      acc
      (rev-acc (rest l) (cons (first l) acc))))

; (rev-acc (list 1 2 3 4 5) empty)
; --> (rev-acc [2, 3, 4, 5] [1])
; --> (rev-acc [3, 4, 5] [2; 1])


(define (rev2 l)
  (rev-acc l empty))

(define (rev-contin l k)
  (if (equal? l empty)
      (k empty)
      (rev-contin (rest l) (lambda (rl) (k (append rl (list (first l))))))))
;                                          (append (rev (rest l)) (list (first l)))))
(define (rev3 l)
  (rev-contin l (lambda (x) x)))

; (rev3 (list 1 2 3)
; (rev-contin [1, 2, 3] (lambda (x) x))
; (rev-contin [2, 3] (lambda (rl) ((lambda (x) x) (append rl (list 1))))
; (rev-contin [3] (lambda (rl) ( (lambda (rl) ((lambda (x) x) (append rl (list 1)))) (append rl (list 2))

; [1; 2; 3; 4; 5]
; --> [f 1; f 2; f 3; f 4; f 5]

(define (map f l)
  (if (equal? l empty)
      empty
      (cons (f (first l)) (map f (rest l)))))

(define (map-contin f l k)
  (if (equal? l empty)
      (k empty)
      (map-contin f (rest l) (lambda (fl) (k (cons (f (first l)) fl))))))

(define (map2 f l)
  (map-contin f l (lambda (x) x)))



      