#lang racket

(define (loop x y)
  (let ([l (box #f)]
        [i (box x)])
  (begin
   (let/cc k (set-box! l k))
   (write (unbox i))
   (newline)
   (set-box! i (+ (unbox i) 1))
   (when (<= (unbox i) y) ((unbox l))))))

(define (iterator f xs)
  (unless (null? xs)
    (begin
      (f (car xs))
      (iterator f (cdr xs)))))

(define (iterator_ext_faulty xs)
  (let ([b (box false)])
    (lambda ()
      (if (unbox b)
          ((unbox b))
          (let/cc k
            (begin
              (iterator (lambda (x) (let/cc next (begin (set-box! b next) (k x)))) xs)))))))


(define gennums (iterator_ext_faulty (list 1 2 3 4)))

(gennums)
(+ 10 (gennums))

(define (iterator_ext_fixed xs)
  (let ([b (box false)]
        [out (box false)])
    (lambda ()
      (let/cc k
      (if (unbox b)
          ((unbox b) k)
          (begin
              (set-box! out k)
              (iterator (lambda (x) (set-box! out (let/cc next (begin (set-box! b next) ((unbox out) x))))) xs)))))))

(define gennums_ok (iterator_ext_fixed (list 1 2 3 4)))