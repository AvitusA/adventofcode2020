#lang racket

(define (parse-input str)
  (let* ([deck-strs (string-split str "\n\n")]
         [p1-deck (map string->number ((compose (curryr drop 1)
                                                (curryr string-split "\n")
                                                first) deck-strs))]
         [p2-deck (map string->number ((compose (curryr drop 1)
                                                (curryr string-split "\n")
                                                second) deck-strs))])
    (values p1-deck p2-deck)))

(define/match (round1 p1-deck p2-deck) ;; Part 1
  [((list* p1-head p1-rest) (list* p2-head p2-rest))
   (if (p1-head . > . p2-head)
       (round1 (append p1-rest (list p1-head p2-head)) p2-rest) ;; p1 win
       (round1 p1-rest (append p2-rest (list p2-head p1-head))))] ;; p2 win
  [(p1-deck p2-deck) #:when (empty? p2-deck) p1-deck] ;; p1 win
  [(p1-deck p2-deck) #:when (empty? p1-deck) p2-deck]) ;; p2 win

(define (round2 p1-deck p2-deck seen-decks) ;; Part 2
  (let ([p1-win (λ () (round2 (append (cdr p1-deck) (list (car p1-deck) (car p2-deck)))
                              (cdr p2-deck)
                              (set-add seen-decks (cons p1-deck p2-deck))))]
        [p2-win (λ () (round2 (cdr p1-deck)
                              (append (cdr p2-deck) (list (car p2-deck) (car p1-deck)))
                              (set-add seen-decks (cons p1-deck p2-deck))))])
    (cond
      [(set-member? seen-decks (cons p1-deck p2-deck)) (cons 'p1 p1-deck)]
      [(empty? p2-deck) (cons 'p1 p1-deck)] ;; Round wins
      [(empty? p1-deck) (cons 'p2 p2-deck)]
      [(and ((car p1-deck) . <= . (length (cdr p1-deck))) ;; Both can recurse
            ((car p2-deck) . <= . (length (cdr p2-deck))))
       (match (game (take (cdr p1-deck) (car p1-deck))
                    (take (cdr p2-deck) (car p2-deck)))
         [(list* 'p1 _) (p1-win)]
         [(list* 'p2 _) (p2-win)])]
      [((car p1-deck) . >= . (car p2-deck)) (p1-win)]
      [((car p1-deck) . <= . (car p2-deck)) (p2-win)])))

(define (game p1-deck p2-deck)
  (round2 p1-deck p2-deck (set)))

(define (score deck)
  (for/fold ([acc 0])
            ([card (reverse deck)]
             [multiplier (in-naturals 1)])
    (+ acc (* multiplier card))))

(define-values (p1-deck p2-deck) (parse-input (with-input-from-file "input" port->string)))

;; Part 1
(displayln (score (round1 p1-deck p2-deck)))

;; Part2
(displayln (score (cdr (game p1-deck p2-deck))))
