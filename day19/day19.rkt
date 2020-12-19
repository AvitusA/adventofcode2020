#lang racket

(define/match (parse-rule str)
  [((pregexp #px"^(\\d+): (?:\"([a-z])\")$"
             (list _ rule-id char)))
   (cons (string->number rule-id) (string-ref char 0))]
  [((pregexp #px"^(\\d+): ((\\d+( |$))+)$"
             (list _ rule-id sequence _ _)))
   (cons (string->number rule-id)
         (list (map string->number (string-split sequence " "))))]
  [((pregexp #px"^(\\d+): ((\\d+(?: ))+)\\| ((\\d+( |$))+)$"
             (list _ rule-id sequence1 _ sequence2 _ _)))
   (cons (string->number rule-id)
         (list (map string->number (string-split sequence1 " "))
               (map string->number (string-split sequence2 " "))))])

(define (parse-rules input-str)
  (make-immutable-hash (map parse-rule ((compose (curryr string-split "\n")
                                                 first
                                                 (curryr string-split "\n\n"))
                                        input-str))))

(define (parse-candidates input-str)
  ((compose (curryr string-split "\n")
            second
            (curryr string-split "\n\n")) input-str))

(define (apply-rule str rules rulebook max-depth)
  (cond
    [(= 0 (length rules) (string-length str)) #t]
    [(xor (= 0 (length rules)) (= 0 (string-length str))) #f]
    [(equal? max-depth 0) #f]
    [else (match (hash-ref rulebook (car rules))
            [c #:when (char? c) (if (equal? c (string-ref str 0))
                                      (apply-rule (substring str 1) (cdr rules) rulebook (sub1 max-depth))
                                      #f)]
            [l #:when (list? l)
               (ormap (λ (sequence) (apply-rule str (append sequence (cdr rules)) rulebook (sub1 max-depth))) l)])]))

;; Part 1 & 2 (use different input)
(define puzzle-input (port->string))
(define rulebook (parse-rules puzzle-input))
(define candidates (parse-candidates puzzle-input))
(displayln (count (curryr apply-rule (list 0) rulebook 1000) candidates))

