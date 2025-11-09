#lang racket

(let/cc k
  (k 3))
  
(+ 1 (let/cc k (k 3)))

(define (f n)
  (+ 10
     (* 5 (let/cc k (/ 1 (if (zero? n) (k 1) n))))))

(f 0)
(f 2)

(define c (box #f))

(+ 1 (+ 2 (+ 3 (+ (let/cc k (set-box! c k) 4) 5))))

((unbox c) 100)

(define generate-numbers-1
  (let ([b (box false)])
    (lambda (send)
        (if (unbox b)
            ((unbox b) '())
            (begin
             (let/cc k (set-box! b k) (send 1))
             (let/cc k (set-box! b k) (send 2))
             (let/cc k (set-box! b k) (send 3))
             (send 4))))))

; (let/cc k (generate-numbers-1 k))
; (let/cc k (generate-numbers-1 k))
; (let/cc k (generate-numbers-1 k))
; (let/cc k (generate-numbers-1 k))


(define generate-numbers-2
  (let ([b (box false)])
    (lambda (outer-send)
      (let ([send (lambda (v) (let/cc k (set-box! b k) (outer-send v)))])
        (if (unbox b)
            ((unbox b) '())
            (begin
             (send 1)
             (send 2)
             (send 3)
             (send 4)))))))

(let/cc k (generate-numbers-2 k))
(let/cc k (generate-numbers-2 k))
(let/cc k (generate-numbers-2 k))
(let/cc k (generate-numbers-2 k))

;(define (get producer) (let/cc k (displayln k) (producer k)))
;(let* ([a1 (let/cc k (generate-numbers-1 k))]
;       [a2 (let/cc k (generate-numbers-1 k))]
;       [a3 (let/cc k (generate-numbers-1 k))])
;       (list a1 a2 a3))

(define generate-numbers-3
  (let ([b (box false)])
    (lambda (outer-send)
      (let* ([send-to (box outer-send)]
             [send (lambda (v) (set-box! send-to (let/cc k (set-box! b k) ((unbox send-to) v))))])
        (if (unbox b)
            ((unbox b) outer-send)
            (begin
             (send 1)
             (send 2)
             (send 3)
             (send 4)))))))

(define (get producer) (let/cc k (producer k)))
(let* ([a1 (let/cc k (generate-numbers-3 k))]
       [a2 (let/cc k (generate-numbers-3 k))]
       [a3 (let/cc k (generate-numbers-3 k))])
       (list a1 a2 a3))


(define (generate-producer body)
  (let ([b (box false)])
    (lambda (outer-send)
      (let* ([send-to (box outer-send)]
             [send (lambda (v) (set-box! send-to (let/cc k (set-box! b k) ((unbox send-to) v))))])
        (if (unbox b)
            ((unbox b) outer-send)
            (body send))))))

(define (generate-numbers-body-4 send)
            (begin
             (send 1)
             (send 2)
             (send 3)
             (send 4)))

(define generate-numbers-4
  (generate-producer generate-numbers-body-4))
  
(list (get generate-numbers-4)
      (get generate-numbers-4)
      (get generate-numbers-4))
