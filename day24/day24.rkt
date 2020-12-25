#lang racket

(struct v2 (x y) #:transparent)
(define (v2-add lhs rhs)
  (v2 (+ (v2-x lhs) (v2-x rhs))
      (+ (v2-y lhs) (v2-y rhs))))


(define (parse-path path)
  (letrec ([helper (λ (x acc)
                     (match x
                       [(list* #\s #\e xs) (helper xs (cons 'se acc))]
                       [(list* #\s #\w xs) (helper xs (cons 'sw acc))]
                       [(list* #\n #\e xs) (helper xs (cons 'ne acc))]
                       [(list* #\n #\w xs) (helper xs (cons 'nw acc))]
                       [(list* #\w xs) (helper xs (cons 'w acc))]
                       [(list* #\e xs) (helper xs (cons 'e acc))]
                       [(list) (reverse acc)]))])
    (helper (string->list path) (list))))

(define (move dir from)
  (match dir
    ['w (v2-add from (v2 -1 0))]
    ['e (v2-add from (v2 1 0))]
    ['se #:when (even? (v2-y from)) (v2-add from (v2 0 -1))]
    ['se #:when (odd? (v2-y from)) (v2-add from (v2 1 -1))]
    ['sw #:when (even? (v2-y from)) (v2-add from (v2 -1 -1))]
    ['sw #:when (odd? (v2-y from)) (v2-add from (v2 0 -1))]
    ['ne #:when (even? (v2-y from)) (v2-add from (v2 0 1))]
    ['ne #:when (odd? (v2-y from)) (v2-add from (v2 1 1))]
    ['nw #:when (even? (v2-y from)) (v2-add from (v2 -1 1))]
    ['nw #:when (odd? (v2-y from)) (v2-add from (v2 0 1))]))

(define (neighbors tile)
  (map (curryr move tile) (list 'e 'se 'sw 'w 'nw 'ne)))

(define (follow-path from path)
  (foldl move from path))

(define (flip-paths paths)
  (let ([endpoints (map (curry follow-path (v2 0 0)) paths)])
    (foldl (λ (x black-tiles)
             (if (set-member? black-tiles x)
                 (set-remove black-tiles x)
                 (set-add black-tiles x)))
           (set)
           endpoints)))

(define (count-black-neighbors black-tiles tile)
  (count (curry set-member? black-tiles) (neighbors tile)))

(define (rule black-tiles tile) ;; -> tile iff it is/becomes black, else #f
  (let ([n-black-neighbors (count-black-neighbors black-tiles tile)])
    (cond [(set-member? black-tiles tile) (if (or (n-black-neighbors . = . 0)
                                                  (n-black-neighbors . > . 2))
                                              #f
                                              tile)]
          [else (if (n-black-neighbors . = . 2) tile #f)])))

(define (tick black-tiles)
  (let* ([simulated-tiles (set-union black-tiles
                                     ((compose (curry apply set)
                                                    flatten
                                                    (curryr set-map neighbors))
                                           black-tiles))]
         [black-tiles-post ((compose (curry apply set)
                                     (curry filter identity)
                                     (curryr set-map (curry rule black-tiles)))
                            simulated-tiles)])
    black-tiles-post))

(define puzzle-input (with-input-from-file "input" port->lines))

;; Part 1:
(define tiles-after-part1 (flip-paths (map parse-path puzzle-input)))
(displayln (set-count tiles-after-part1))

;; Part 2:
(displayln (set-count (for/fold ([state tiles-after-part1])
                                ([day (in-range 100)])
                        (tick state))))

