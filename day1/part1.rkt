#lang racket

;; Read stdin to EOF
(define puzzle-input (map string->number (string-split (port->string))))
(define matching-numbers (findf (lambda (pair)
                                  (equal? 2020 (+ (first pair) (second pair))))
                                (cartesian-product puzzle-input puzzle-input)))
(displayln
 (if matching-numbers
     (* (first matching-numbers) (second matching-numbers))
     ("Error: No match found")))

