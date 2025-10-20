#lang racket

; Exploring in Racket how we can implement recursion through state


; (let ([b b])
;   b)


(let ([b (box 'dummy)])
  (begin
    (set-box! b b)
    b))

(let* ([b1 (box 'dummy)]
      [b2 (box b1)])
  (begin
    (set-box! b1 b2)
    b1))

; (let ([fact (lambda (n)
;               (if (equal? 0 n)
;                    1
;                    (* n (fact (- n 1)))))])
;   (fact 5))

(let* ([fact (box 'dummy)]
       [fact-fun
        (lambda (n)
              (if (equal? 0 n)
                   1
                   (* n ((unbox fact) (- n 1)))))])
  (begin
    (set-box! fact fact-fun)
    ((unbox fact) 5)))

(define fix
  (lambda (f)
    (let* ([b (box 'dummy)]
           [frec (lambda (x)
                   (f (unbox b) x))])
      (begin
        (set-box! b frec)
        frec))))

(define fact-alt
  (fix (lambda (rec n)
         (if (equal? 0 n)
             1
             (* n (rec (- n 1)))))))
