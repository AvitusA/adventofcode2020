#lang racket

(struct seatmap (seats neighbors) #:transparent)

(define (line x d t)
  (cons (+ (car x) (* t (car d)))
        (+ (cdr x) (* t (cdr d)))))

(define (adj-neighbors seats pos)
  (let ([max-row (apply max (map car (hash-keys seats)))]
        [max-col (apply max (map cdr (hash-keys seats)))])
    (filter (λ (x) (and (<= 0 (car x) max-row)
                        (<= 0 (cdr x) max-col)
                        (not (equal? pos x))))
            (map (λ (x) (line pos (cons (first x) (second x)) 1))
                 (cartesian-product '(-1 0 1) '(-1 0 1))))))


(define (los-neighbors seats pos)
  (let* ([directions (filter (compose not (curry equal? (cons 0 0)))
                             (map (λ (x) (cons (first x) (second x)))
                                  (cartesian-product '(-1 0 1) '(-1 0 1))))]
         [max-row (apply max (map car (hash-keys seats)))]
         [max-col (apply max (map cdr (hash-keys seats)))])
    (filter identity
            (map (λ (direction)
                   (for/first ([t (in-naturals 1)]
                               #:break (not (and (<= 0 (car (line pos direction t)) max-row)
                                                 (<= 0 (cdr (line pos direction t)) max-col)))
                               #:when (member (hash-ref seats (line pos direction t)
                                                        #f)
                                              (list #\L #\#)))
                     (line pos direction t)))
                 directions))))

(define (rule the-seatmap pos occupied-threshold)
  (let ([neighbors (map (curry hash-ref (seatmap-seats the-seatmap))
                        (hash-ref (seatmap-neighbors the-seatmap) pos))])
    (match (hash-ref (seatmap-seats the-seatmap) pos)
      [#\L #:when (andmap (curry (compose not equal?) #\#) neighbors) #\#]
      [#\# #:when ((count (curry equal? #\#) neighbors) . >= . occupied-threshold) #\L]
      [x x])))

(define (tick the-seatmap the-rule)
  (seatmap (make-immutable-hash
            (map (λ (x) (cons x (the-rule the-seatmap x)))
                 (hash-keys (seatmap-seats the-seatmap))))
           (seatmap-neighbors the-seatmap)))           

(define (tick-until-stationary the-seatmap the-rule)
  (let ([next-seatmap (tick the-seatmap the-rule)])
    (if (equal? the-seatmap next-seatmap)
        the-seatmap
        (tick-until-stationary next-seatmap the-rule))))

(define (make-seatmap str neighbor-proc)
  (let* ([seats
          (make-immutable-hash (for/fold ([acc '()])
                                         ([row (string-split str "\n")]
                                          [row-num (in-naturals)])
                                 (append acc (for/list ([pos (string->list row)]
                                                        [col-num (in-naturals)])
                                               (cons (cons row-num col-num) pos)))))]
         [neighbors (make-immutable-hash
                     (map (λ (x) (cons x (neighbor-proc seats x)))
                          (hash-keys seats)))])
    (seatmap seats neighbors)))


(define (count-steady-occupied str neighbor-proc occupied-threshold)
  (count (curry equal? #\#)
         (hash-values (seatmap-seats (tick-until-stationary
                                      (make-seatmap str
                                                    neighbor-proc)
                                      (curryr rule occupied-threshold))))))

;; Part 1
(displayln (count-steady-occupied (port->string) adj-neighbors 4))

;; Part 2
(displayln (count-steady-occupied (port->string) los-neighbors 5))

