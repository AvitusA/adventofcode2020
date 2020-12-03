#lang racket

(define course (map string->list (string-split (port->string) "\n")))
(define course-width (length (car course)))
(define course-length (length course))

(define (get-cell col row)
  (let ([wrapped-col (modulo col course-width)])
    (list-ref (list-ref course row) wrapped-col)))

(define (slope-points dcol drow)
  (map (lambda (row) (list (* (/ row drow) dcol) row)) ; '(col row)
       (sequence->list (in-range 0 course-length drow))))

(define (count-trees points)
  (count (lambda (point) (equal? (apply get-cell point) #\#)) points))

(define slopes '((1 1) (3 1) (5 1) (7 1) (1 2)))

(displayln
 (foldl (lambda (slope acc) (* acc (count-trees (apply slope-points slope))))
        1 slopes))
