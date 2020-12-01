#lang racket
(define puzzle-input (map string->number (string-split (port->string))))
(define matching-numbers (findf (lambda (lst)
                                  (equal? 2020 (foldl + 0 lst)))
                                (cartesian-product puzzle-input
                                                   puzzle-input
                                                   puzzle-input)))
(displayln (if matching-numbers
               (foldl * 1 matching-numbers)
               ("Error: No match found")))
