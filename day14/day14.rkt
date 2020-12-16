#lang racket

(struct state (current-mask memory) #:transparent)

(define initial-state (state 0 (make-immutable-hash)))

(define (tick write-proc instruction the-state)
  (match instruction
    [(regexp #px"(?<=mask = )(0|1|X){36}" (list mask _))
     (struct-copy state the-state [current-mask mask])]
    [(regexp #px"(?:mem\\[)(\\d+)(?:\\] = )(\\d+)" (list _ address value))
     (write-proc the-state (string->number address) (string->number value))]))

(define (tick-multiple write-proc instructions the-state)
  (foldl (curry tick write-proc) the-state instructions))

(define puzzle-input (port->lines))

;; Part 1
(define (write1 the-state address value)
  (let* ([orbits (string->number (string-replace (state-current-mask the-state) "X" "0") 2)]
         [andbits (string->number (string-replace (state-current-mask the-state) "X" "1") 2)]
         [written-value (bitwise-and andbits (bitwise-ior orbits value))])
    (struct-copy state the-state
                 [memory (hash-set (state-memory the-state) address written-value)])))

(displayln (foldl + 0 (hash-values (state-memory (tick-multiple write1 puzzle-input initial-state)))))

;; Part 2
(define (bitmask on-bits)
  (foldl (λ (bit acc) (bitwise-ior acc (arithmetic-shift 1 bit))) 0 on-bits))

(define (make-addresses mask address)
  (let* ([floating-positions (indexes-where (reverse (string->list mask)) (curry equal? #\X))]
         [floating-position-mask (bitwise-not (bitmask floating-positions))]
         [floating-combinations (map bitmask (combinations floating-positions))]
         [base-address (bitwise-ior (string->number (string-replace mask "X" "0") 2) address)])
    (map (λ (x) (bitwise-ior x (bitwise-and base-address floating-position-mask)))
         floating-combinations)))

(define (write2 the-state address value)
  (let ([addresses (make-addresses (state-current-mask the-state) address)])
    (struct-copy state the-state [memory (foldl (λ (x acc)
                                                  (hash-set acc x value))
                                                (state-memory the-state)
                                                addresses)])))

(displayln (foldl + 0 (hash-values (state-memory (tick-multiple write2 puzzle-input initial-state)))))

