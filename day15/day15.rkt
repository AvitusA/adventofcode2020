#lang racket

(define puzzle-input '(20 0 1 11 6 3))

(define initial-memory (for/hash ([i (take puzzle-input (sub1 (length puzzle-input)))]
                                  [turn (in-naturals 1)])
                         (values i turn)))

(define (play acc x) ;; acc = (cons memory last-number)
  (let* ([last-spoken (hash-ref (car acc) (cdr acc) #f)]
         [next (if last-spoken (- (sub1 x) last-spoken) 0)])
    (cons (hash-set (car acc) (cdr acc) (sub1 x)) next)))

(displayln (cdr (sequence-fold play (cons initial-memory 3) (in-range 7 2021))))
(displayln (cdr (sequence-fold play (cons initial-memory 3) (in-range 7 30000001))))

