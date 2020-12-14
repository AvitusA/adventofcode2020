#lang racket

(define puzzle-input (port->lines))

;; Part 1
(define (next-departure earliest bus-id)
  (+ (* bus-id (quotient (- earliest 1) bus-id)) bus-id))

(define (earliest-departure-id earliest bus-ids)
  (argmin (curry next-departure earliest) bus-ids))

(define earliest (string->number (first puzzle-input)))
(define bus-ids (map string->number (filter (compose not (curry equal? "x"))
                                            (string-split (second puzzle-input) ","))))

(let* ([earliest-bus-id (earliest-departure-id earliest bus-ids)]
       [earliest-departure (next-departure earliest earliest-bus-id)])
  (displayln (* (- earliest-departure earliest) earliest-bus-id)))


;; Part 2
(require math/number-theory)

;; Compute x for a = b mod m
(define (solve-mod a b m)
  (when (not (divides? (gcd a m) b)) (error "System lacks solution"))
  (let ([a1 (modulo a m)]
        [b1 (modulo b m)])
    (for/first ([x (in-naturals)] ;; Could solve diophantine equation instead
                #:when (equal? (modulo (* a1 x) m) b1))
      x)))

(define (add-next-bus bus-id offset acc) ;; acc = (cons start step)
  (let* ([start (car acc)]
         [step (cdr acc)]
         [k (solve-mod step (- (+ start offset)) bus-id)])
    (cons (+ (* k step) start) (lcm step bus-id))))

(define (earliest-lineup bus-ids offsets)
  (car (foldl add-next-bus (cons 0 (first bus-ids))
              (rest bus-ids) (rest offsets))))

(define offsets
  (map cdr (filter (compose not (curry equal? "x") car)
                   (for/list ([bus-id (string-split (second puzzle-input) ",")]
                              [offset (in-naturals)])
                     (cons bus-id offset)))))

(displayln (earliest-lineup bus-ids offsets))
