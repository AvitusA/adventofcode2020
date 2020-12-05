#lang racket

(define (bisect lower upper) (+ (quotient (- upper lower) 2) lower))

(define (find-seat seat-chars min-row max-row min-col max-col)
  (match seat-chars
    [(cons #\F xs) (find-seat xs min-row (bisect min-row max-row) min-col max-col)]
    [(cons #\B xs) (find-seat xs (+ 1 (bisect min-row max-row)) max-row min-col max-col)]
    [(cons #\L xs) (find-seat xs min-row max-row min-col (bisect min-col max-col))]
    [(cons #\R xs) (find-seat xs min-row max-row (+ 1 (bisect min-col max-col)) max-col)]
    ['() #:when (and (equal? min-row max-row) (equal? min-col max-col)) (list min-row min-col)]))

(define (seat-id row col) (+ (* 8 row) col))

(define seat-ids (map (λ (seat) (apply seat-id (find-seat seat 0 127 0 7)))
                      (map string->list (string-split (port->string) "\n"))))
;; Part 1
(displayln (apply max seat-ids))

;; Part 2
(displayln (set-subtract (list->set (sequence->list (in-range (apply min seat-ids)
                                                              (+ 1 (apply max seat-ids)))))
                         (list->set seat-ids)))

