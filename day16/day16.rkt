#lang racket
;; Part 1:
(define (parse-rule rule)
  (match rule
    [(pregexp #px"([\\s|\\w]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)"
              (list _ name low1 high1 low2 high2))
     (cons name
           (apply set (append (sequence->list (in-range (string->number low1)
                                                        (add1 (string->number high1))))
                              (sequence->list (in-range (string->number low2)
                                                        (add1 (string->number high2)))))))]))

(define (parse-rules input-str)
  (map parse-rule (string-split (first (string-split input-str "\n\n")) "\n")))

(define (parse-nearby-tickets input-str)
  (map (compose (curry map string->number) (curryr string-split ","))
       (cdr (string-split (third (string-split input-str "\n\n")) "\n"))))

(define (parse-my-ticket input-str)
  (map string->number (string-split (second (string-split (second
                                                           (string-split input-str "\n\n"))
                                                          "\n"))
                                    ",")))

(define (matches-any value rules)
  (ormap (compose (curryr set-member? value) cdr) rules))

(define (scanning-error-rate tickets rules)
  (foldr + 0
         (flatten
          (map (λ (ticket) (filter (λ (value) (not (matches-any value rules))) ticket)) tickets))))

(define puzzle-input (port->string))
(define rules (parse-rules puzzle-input))
(define nearby-tickets (parse-nearby-tickets puzzle-input))
(define my-ticket (parse-my-ticket puzzle-input))

(displayln (scanning-error-rate nearby-tickets rules))

;; Part 2:
(define (filter-invalid-tickets tickets rules)
  (filter (curry andmap (curryr matches-any rules)) tickets))

(define (possible-fields valid-tickets rules)
  (for/list ([col (apply map list valid-tickets)]
             [col-index (in-naturals)])
    (cons col-index
          (filter-map (λ (r) (if (andmap (curry set-member? (cdr r)) col)
                                 (car r)
                                 #f)) rules))))

(define (propagate-units assignments)
  (let* ([units (apply set (filter (compose (curry equal? 2) length) assignments))]
         [unit-assignments (apply set (set-map units second))]
         [propagated-assignments
          (map (λ (x) (if (set-member? units x)
                          x
                          (cons (car x)
                                (filter (compose not (curry set-member? unit-assignments))
                                        (cdr x)))))
               assignments)]
         [unit-counts (set-map unit-assignments
                               (λ (u) (count (λ (assignment) (member u assignment))
                                             propagated-assignments)))])
    (cond
      [(ormap (curry > 1) unit-counts) #f]
      [(equal? assignments propagated-assignments) assignments]
      [else (propagate-units propagated-assignments)])))

(define (departure-fields assignments)
  (filter-map (λ (a) (if (string-contains? (second a) "departure")
                         (car a) #f))
              assignments))

;; Part 2
(define valid-tickets (filter-invalid-tickets nearby-tickets rules))
(define field-assignment-solution (propagate-units (possible-fields valid-tickets rules)))
(define departure-field-values (map (curry list-ref my-ticket)
                                    (departure-fields field-assignment-solution)))

(displayln (foldr * 1 departure-field-values))
