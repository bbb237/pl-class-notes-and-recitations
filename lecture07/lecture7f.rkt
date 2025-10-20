#lang racket

(define o-1
  (lambda (m)
    (case m
      [(add) (lambda (x y) (+ x y))]
      [(add1) (lambda (x) (+ x 1))]
      [(sub1) (lambda (x) (- x 1))])))


((o-1 'add1) 5)
((o-1 'add) 5 6)

(define (msg o m . a)
  (apply (o m) a))


(msg o-1 'add 5 6)


(define (o-constr-1 x)
  (lambda (m)
    (case m
      ((addX) (lambda (y) (+ x y))))))


(define (obj-counter count)
  (let [(b (box count))]
    (lambda (m)
      (case m
        [(inc) (lambda () (set-box! b (+ (unbox b) 1)))]
        [(dec) (lambda () (set-box! b (- (unbox b) 1)))]
        [(get) (lambda () (unbox b))]))))

(define cntr (obj-counter 0))
(define cntr2 (obj-counter 10))
(msg cntr 'inc)
(msg cntr 'inc)
(msg cntr 'dec)
(msg cntr 'get)
(msg cntr2 'inc)
(msg cntr2 'get)
(msg cntr 'get)

; Notice that the internal state of these counter is *private* -- there is no
; way to manipulate that state except through using inc/dec

(define o-static-1
  (let ([numobj 0])
    (lambda (amount)
      (begin
        (set! numobj (+ 1 numobj))
        (lambda (m)
          (case m
            [(inc) (lambda (n) (set! amount (+ amount n)))]
            [(dec) (lambda (n) (set! amount (- amount n)))]
            [(get) (lambda () amount)]
            [(count) (lambda () numobj)]))))))

(let ([o (o-static-1 1000)])
       (msg o 'count))
 
(let ([o (o-static-1 0)])
        (msg o 'count))

(define o-self!
  (let ([self (box 'dummy)])
    (begin
      (set-box! self
            (lambda (m)
              (case m
                [(first) (lambda (x) (msg (unbox self) 'second (+ x 1)))]
                [(second) (lambda (x) (+ x 1))])))
      (unbox self))))

(msg o-self! 'first 5)

; Pass as argument

(define o-self
  (lambda (m)
    (case m
      [(first) (lambda (self x) (msg/self self 'second (+ x 1)))]
      [(second) (lambda (self x) (+ x 1))])))

(define (msg/self o m . a)
  (apply (o m) o a))
      
(msg/self o-self 'first 5)


; In our current set-up the name of the message we invoke is an expression that can be computed

(define test
  (lambda (x)
    (msg o-self! (if (equal? x 1) 'first 'second) 5)))

(define (mt)
  (lambda (m)
    (case m
      [(add) (lambda () 0)])))
 
(define (node v l r)
  (lambda (m)
    (case m
      [(add) (lambda () (+ v
                           (msg l 'add)
                           (msg r 'add)))])))

(define a-tree
  (node 10
        (node 5 (mt) (mt))
        (node 15 (node 6 (mt) (mt)) (mt))))

(msg a-tree 'add)

(define (node/size parent-maker v l r)
  (let ([parent-object (parent-maker v l r)])
    (lambda (m)
          (case m
            [(size) (lambda () (+ 1
                                  (msg l 'size)
                                  (msg r 'size)))]
            [else (parent-object m)]))))
 
(define (mt/size parent-maker)
  (let ([parent-object (parent-maker)])
    (lambda (m)
      (case m
        [(size) (lambda () 0)]
        [else (parent-object m)]))))

(define a-tree/size
  (node/size node
             10
             (node/size node 5 (mt/size mt) (mt/size mt))
             (node/size node 15
                        (node/size node 6 (mt/size mt) (mt/size mt))
                        (mt/size mt))))

(msg a-tree/size 'add)
(msg a-tree/size 'size)
