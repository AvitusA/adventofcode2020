#lang racket

(define (parse-node str)
  (let ([node-name (car (regexp-match #px"^\\w+ \\w+" str))]
        [children (map (λ (x)
                         (match (string-split x " ")
                           [(list n name1 name2)
                            (cons (string-append name1 " " name2)
                                  (string->number n))]))
                       (regexp-match* #px"(\\d+) (\\w+ \\w+)(?= bag)" str))])
    (cons node-name children)))

(define node-strings (string-split (port->string) "\n"))
(define available-colors (map (compose car parse-node) node-strings))
(define the-graph (make-hash (map parse-node node-strings)))

;; Part 1
(define (can-contain? graph containee container)
  (match (hash-ref graph container '())
    [(list-no-order (cons child _) _ ...) #:when (equal? child containee) #t]
    ['() #f]
    [children (ormap (compose (curry can-contain? graph containee) car) children)]))
(displayln (count (curry can-contain? the-graph "shiny gold") available-colors))

;; Part 2
(define (bags-required graph container)
  (match (hash-ref graph container '())
    ['() 1] ;; This bag only
    [children (+ 1 (foldl + 0
                          (map (λ (child) (* (cdr child)
                                             (bags-required graph (car child))))
                               children)))]))
(displayln ((bags-required the-graph "shiny gold") . - . 1))
