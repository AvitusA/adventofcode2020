#lang racket

(define all-answers (map (compose (λ (x) (map string->list x))
                                  (λ (x) (string-split x "\n")))
                         (string-split (port->string) "\n\n")))

;; Part 1:
(define (group-answers-any group-answers)
  (foldl (λ (x acc) (set-add acc x)) (set) (flatten group-answers)))

(displayln (foldl + 0 (map (compose set-count group-answers-any) all-answers)))

;; Part 2:
(define (group-answers-all group-answers)
  (foldl (λ (x acc) (set-intersect acc (list->set x)))
         (list->set (first group-answers))
         (rest group-answers)))

(displayln (foldl + 0 (map (compose set-count group-answers-all) all-answers)))
