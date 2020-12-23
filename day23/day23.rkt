#lang racket

(define sample-input (list 3 8 9 1 2 5 4 6 7))
(define puzzle-input (list 1 3 5 4 6 8 7 2 9))
(define n-cups (length puzzle-input))

(define (move cups current-cup)
  (let* ([current-cup-idx (index-of cups current-cup)]
         [picked-up-cups (for/list ([idx (in-naturals (add1 current-cup-idx))]
                                    [n (in-range 3)])
                           (list-ref cups (modulo idx n-cups)))]
         [remaining-cups (filter (compose not (curryr member picked-up-cups)) cups)]
         [destination-cup (findf (curryr member remaining-cups)
                                 (append (range (sub1 current-cup) 0 -1)
                                         (list (apply max remaining-cups))))]
         [destination-cup-idx (index-of remaining-cups destination-cup)]
         [result-order (append (take remaining-cups (add1 destination-cup-idx))
                               picked-up-cups
                               (drop remaining-cups (add1 destination-cup-idx)))]
         [next-current (list-ref result-order
                                 (modulo (add1 (index-of result-order current-cup)) n-cups))])
    (printf "cups: ~a~n" cups)
    (printf "current cup: ~a~n" current-cup)
    (printf "pickup: ~a~n" picked-up-cups)
    (printf "dest: ~a~n" destination-cup)
    (printf "dest-idx: ~a~n" destination-cup-idx)
    (printf "next current: ~a~n" next-current)
    (printf "next current idx: ~a~n~n" (modulo (add1 (index-of result-order current-cup)) n-cups))
    (values result-order next-current)))

(define (play cups current-cup n)
  (let-values ([(result-cups next-current-cup) (move cups current-cup)])
    (if (n . > . 1)
        (play result-cups next-current-cup (sub1 n))
        result-cups)))

(define (crabstring order)
  (string-join (map number->string (for/list ([idx (in-naturals (add1 (index-of order 1)))]
                                              [n (in-range (sub1 n-cups))])
                                     (list-ref order (modulo idx n-cups)))) ""))
        
(displayln (crabstring (play puzzle-input (car puzzle-input) 100)))
