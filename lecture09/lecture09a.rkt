#lang racket


; (/ 1 0)

; (car 17)

; (error 'myfunction "Something bad happened!")

; (with-handlers ([predicate handler] ...) body)

(with-handlers ([exn:fail? (lambda (exn) "We're handling the bad thing.")])
  (error 'myfunction "Something bad happened!"))

(with-handlers ([exn:fail? (lambda (exn) "We're handling the bad thing.")])
  (/ 1 0))

(with-handlers ([exn:fail? (lambda (exn) "We're handling the bad thing.")])
  (displayln "Started running!")
  (error 'myfunction "Something bad happened!")
  (displayln "Never got here.")
  )


(with-handlers ([exn:fail:contract:divide-by-zero? (lambda (exn) "Division by 0")]
                [exn:fail? (lambda (exn) "We're handling the bad thing.")])
  (/ 1 0))


(with-handlers ([(lambda (v) #t) (lambda (n) (displayln n))])
  (raise 27))

(with-handlers ([even? (lambda (n) (displayln "It was even."))]
                [odd? (lambda (n) (displayln "It was odd."))]
                )
  (raise 27))

; (with-handlers ([even? (lambda (n) (displayln "It was even."))]
;                 )
;   (raise 17))
