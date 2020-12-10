#lang racket

(define puzzle-input
  (sort (cons 0 (map string->number (string-split (port->string) "\n"))) >))

(displayln
 (hash-ref
  (foldl (λ (x acc) (let ([child-paths-from-x (+ (+ (hash-ref acc (+ x 1) 0)
                                                    (hash-ref acc (+ x 2) 0)
                                                    (hash-ref acc (+ x 3) 0)))])
                      (hash-set acc x child-paths-from-x)))
         (make-immutable-hash (list (cons (first puzzle-input) 1)))
         (drop (sort puzzle-input >) 1))
  0))
