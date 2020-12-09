#lang racket

(define (find-first input history preamble-length acc)
  (cond
    [((length history) . <  . preamble-length)
     (find-first (cdr input) (cons (car input) history) preamble-length (+ 1 acc))]
    [(findf (curry equal? (car input))
            (map (curry apply +) (combinations history 2)))
     (find-first (cdr input) (take (cons (car input) history) preamble-length) preamble-length (+ 1 acc))]
    [else acc]))

(define puzzle-input (map string->number (string-split (port->string) "\n")))

;; Part 1
(displayln (list-ref puzzle-input (find-first puzzle-input '() 25 0)))

;; Part 2
(define (find-contiguous-set input sum)
  (let* ([candidates (filter (λ (start-n) ((foldl + 0 start-n) . < . (length input)))
                             (cartesian-product (range (length input))
                                                (range (+ 1 (length input)))))]
         [first-match (findf (λ (start-n) (equal? sum
                                                  (foldl + 0 (take (drop input (second start-n)) (first start-n)))))
                             candidates)]
         [matching-range (take (drop input (second first-match)) (first first-match))])
    (+ (apply min matching-range)
       (apply max matching-range))))

(define (p2)
  (let* ([first-invalid-index (find-first puzzle-input '() 25 0)]
         [first-invalid-value (list-ref puzzle-input first-invalid-index)])
    (find-contiguous-set (take puzzle-input first-invalid-index) first-invalid-value)))

(displayln (p2))
