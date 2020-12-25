#lang racket

(define pkey1 9033205)
(define pkey2 9281649)

(define (transform value subject-number)
  (remainder (* value subject-number) 20201227))

(define (loop subject-number n)
  (for/fold ([value 1])
            ([i (in-range n)])
    (transform value subject-number)))

(define (bf-loop-size pkey subject-number)
  (for/fold ([value-loop-size (cons 1 0)])
             ([loop-size (in-naturals 1)]
              #:break (equal? pkey (car value-loop-size)))
             (cons (transform (car value-loop-size) subject-number)
                   loop-size)))

(displayln (loop pkey2 (cdr (bf-loop-size pkey1 7))))
