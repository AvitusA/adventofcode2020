#lang racket

(define example-map (string-join '("L.LL.LL.LL"
                                   "LLLLLLL.LL"
                                   "L.L.L..L.."
                                   "LLLL.LL.LL"
                                   "L.LL.LL.LL"
                                   "L.LLLLL.LL"
                                   "..L.L....."
                                   "LLLLLLLLLL"
                                   "L.LLLLLL.L"
                                   "L.LLLLL.LL") "\n"))

(define (make-seatmap str)
  (for/list ([row (string-split str "\n")]
             [row-num (in-naturals 1)])
    (for/list ([pos (string->list row)]
               [col-num (in-naturals 1)])
      (cons (cons row-num col-num) pos))))

(define (floor-positions seatmap)
  (map car (filter (compose (curry equal? #\.) cdr) (apply append seatmap))))                 

(define (format-state x)
  (match x
    [#\L "F"]
    [#\. "E"]
    [#\# "O"]))

(define (format-row row)
  (string-join (map (compose format-state cdr) row) ", "))

(define (format-dzn str)
  (let* ([seatmap (make-seatmap str)]
         [floors (floor-positions seatmap)]
         [nrow (length seatmap)]
         [ncol (length (car seatmap))])
    (string-append
;;           "seat_state = {F, E, O};\n"
           "ncol = " (~a ncol) ";\n"
           "nrow = " (~a nrow) ";\n"
           "nfloor = " (~a (length floors)) ";\n"
;;           "seat = ["
;;           (string-join (map (compose (curry string-append "|") format-row) seatmap) "\n\t")
;;           "|];\n"
           "floor_row = [" (string-join (map (compose ~a car) floors) ", ") "];\n"
           "floor_col = [" (string-join (map (compose ~a cdr) floors) ", ") "];\n"
           )))
           ;; (append 
           ;;  (map (λ (x)
           ;;         (string-append
           ;;          "seat[" (~a (caar x)) ", " (~a (cdar x)) "] = " (format-state x) ";\n"))
           ;;       (apply append seatmap))
           ;;  (for/list ([i (in-naturals 1)]
           ;;             [floor-pos floors])
           ;;    (string-append
           ;;     "floor_row[" (~a i) "] = " (~a (car floor-pos)) ";\n"
           ;;     "floor_col[" (~a i) "] = " (~a (cdr floor-pos)) ";\n"))))))

(displayln (format-dzn (port->string)))
