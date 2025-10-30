#lang racket

(define (my-or1 a b)
  (if a #t b))

; (my-or1 #f #t)
; (my-or1 #t #t)
; (my-or1 #f #f)
; (my-or1 #t (print "Hello world!")) ; oops -- no good!

(define-syntax (my-or stx)
  (syntax-case stx ()
    [(my-or a b)
     #'(if a #t b)]))

(define-syntax (multi-or x)
  (syntax-case x ()
    [(multi-or e0 e1 ...)
     #'(let [(b e0)]
            (if b b (multi-or e1 ...)))]
    [(multi-or)
     #'#f]))

(multi-or #t #f #f)
(multi-or #f #t #f)
(multi-or #f #f #f)
(multi-or #t (println "Hello from or!") #f)
(multi-or #f (begin (println "Hello from or2!") #t) #f)

(define-syntax (my-let1 stx)
  (syntax-case stx ()
    [(my-let1 ([var val]) body)
     #'((lambda (var) body) val)]))

(my-let1 [(x 1)] (+ x x))

(define-syntax (my-let* stx)
  (syntax-case stx ()
    [(my-let* ([var val]) body)
     #'((lambda (var) body) val)]
    [(my-let* ([var val] . rest) body)
     #'((lambda (var) (my-let* rest body)) val)]))

(my-let* [(x 1) (y x)] (+ x y))
(my-let* [(x 1) (y x) (z y)] (+ x y))
(let ([x 2]) (my-let* ([x 1] [y x]) (+ x y)))

(define-syntax (my-let stx)
  (syntax-case stx ()
    [(my-let ([var val] ...) body)
     #'((lambda (var ...) body) val ...)]))

(let ([x 2]) (my-let ([x 1] [y x]) (+ x y)))

(define-syntax (swap2 stx)
  (syntax-case stx ()
    [(swap2 x y)
     #'(let [(t y)]
         (set! y x)
         (set! x t))]))

(define a 10)
(define b 20)
(swap2 a b)

(printf "a's value is ~a\n" a)
(printf "b's value is ~a\n" b)
