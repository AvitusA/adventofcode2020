#lang racket

(struct ship (x y dir) #:transparent)

(define (rotate the-ship theta)
  (struct-copy ship the-ship
               [dir (* (expt (make-rectangular 0 1) (quotient theta 90))
                       (ship-dir the-ship))]))

(define (translate-ship the-ship dx dy)
  (struct-copy ship the-ship
               [x (+ dx (ship-x the-ship))]
               [y (+ dy (ship-y the-ship))]))

(define (translate-wp the-ship dx dy)
  (struct-copy ship the-ship
               [dir (+ (make-rectangular dx dy) (ship-dir the-ship))]))

(define (forward the-ship d)
  (struct-copy ship the-ship
               [x (+ (* d (real-part (ship-dir the-ship))) (ship-x the-ship))]
               [y (+ (* d (imag-part (ship-dir the-ship))) (ship-y the-ship))]))

(define (manhattan-dist the-ship)
  (+ (abs (ship-x the-ship)) (abs (ship-y the-ship))))

(define (tick translate-proc instruction-str state)
  (match (cons (substring instruction-str 0 1)
               (string->number (substring instruction-str 1)))
    [(cons "N" dy) (translate-proc state 0 dy)]
    [(cons "S" ndy) (translate-proc state 0 (- ndy))]
    [(cons "E" dx) (translate-proc state dx 0)]
    [(cons "W" ndx) (translate-proc state (- ndx) 0)]
    [(cons "L" theta) (rotate state theta)]
    [(cons "R" ntheta) (rotate state (- ntheta))]
    [(cons "F" d) (forward state d)]))

(define (tick-all translate-proc instruction-strs state)
  (foldl (curry tick translate-proc) state (string-split instruction-strs "\n")))

(define puzzle-input (port->string))

;; Part 1
(define initial-state1 (ship 0 0 1))
(displayln (manhattan-dist (tick-all translate-ship puzzle-input initial-state1)))

;; Part 2
(define initial-state2 (ship 0 0 (make-rectangular 10 1)))
(displayln (manhattan-dist (tick-all translate-wp puzzle-input initial-state2)))

