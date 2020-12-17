#lang racket

(struct vec4 (x y z w) #:transparent)

(define (vec4-add lhs rhs) (vec4 (+ (vec4-x lhs) (vec4-x rhs))
                                 (+ (vec4-y lhs) (vec4-y rhs))
                                 (+ (vec4-z lhs) (vec4-z rhs))
                                 (+ (vec4-w lhs) (vec4-w rhs))))

(define (neighbors4 p)
  (let ([relative-neighbors (map (curry apply vec4)
                                 (filter (compose not (curry equal? (list 0 0 0 0)))
                                         (cartesian-product (list -1 0 1)
                                                            (list -1 0 1)
                                                            (list -1 0 1)
                                                            (list -1 0 1))))])
    (map (curry vec4-add p) relative-neighbors)))

(define (neighbors3 p)
  (filter (λ (n) (equal? (vec4-w p) (vec4-w n))) (neighbors4 p)))

(define (active-neighbors p universe neighbor-proc)
  (count (curryr (curry hash-ref universe) #f) (neighbor-proc p)))

(define (simulated-points universe neighbor-proc)
  (set->list (apply set (apply append (map (compose neighbor-proc car) (hash->list universe))))))

(define (rule1 p universe neighbor-proc)
  (let ([active-neighbors (active-neighbors p universe neighbor-proc)])
    (match (hash-ref universe p #\.)
      [#\# #:when (not (<= 2 active-neighbors 3)) #\.]
      [#\. #:when (equal? 3 active-neighbors) #\#]
      [state state])))

(define (tick universe neighbor-proc)
  (make-immutable-hash
   (filter-map
    (λ (p) (let ([new-state (rule1 p universe neighbor-proc)])
             (if (equal? new-state #\#)
                 (cons p new-state)
                 #f))) (simulated-points universe neighbor-proc))))

(define (parse-input lines)
  (make-immutable-hash
   (filter (compose (curry equal? #\#) cdr)
           (apply append
                  (for/list ([l lines]
                             [y (in-naturals)])
                    (for/list ([c (string->list l)]
                               [x (in-naturals)])
                      (cons (vec4 x y 0 0) c)))))))

(define (multi-tick n initial-state rule)
  (for/fold ([state initial-state])
            ([i (in-range 0 n)])
    (tick state rule)))

(define puzzle-input (port->lines))

;; Part 1
(displayln (hash-count (multi-tick 6 (parse-input puzzle-input) neighbors3)))

;; Part 2
(displayln (hash-count (multi-tick 6 (parse-input puzzle-input) neighbors4)))
