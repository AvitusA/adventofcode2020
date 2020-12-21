#lang racket

(define (parse-input str)
  (let* ([lines (string-split str "\n")]
         [size (string->number (first lines))]
         [row-major-input (map string->number (string-split
                                               (string-trim (second lines) #:left? "[" #:right? "]")
                                               ", "))])
    (for/hash ([c row-major-input]
               [pos (in-naturals)])
      (values
       (list (quotient pos size) (modulo pos size)) ;; (row . column)
       c))))

(define image (parse-input (port->string)))

(define (rot90 im)
  (let ([size (add1 (apply max (map car (hash-keys im))))])
    (for/hash ([p (cartesian-product (range size) (range size))])
      (values
       p
       (hash-ref im (list (- (sub1 size) (cadr p))
                          (car p)))))))

(define (hflip im)
  (let ([size (add1 (apply max (map car (hash-keys im))))])
    (for/hash ([p (cartesian-product (range size) (range size))])
      (values
       p
       (hash-ref im (list (car p)
                          (- (sub1 size) (cadr p))))))))

(define (group im)
  (map (λ (x) (x im))
       (map (curry apply compose) (cartesian-product
                                   (list identity hflip)
                                   (list identity rot90
                                         (compose rot90 rot90)
                                         (compose rot90 rot90 rot90))))))
(define (show im)
  (let* ([size (add1 (apply max (map car (hash-keys im))))])
    (for ([i (range (* size size))])
      (let ([row (quotient i size)]
            [col (modulo i size)])
        (when (equal? col 0)
          (printf "~n"))
        (printf "~a" (hash-ref im (list row col)))))))

(define sea-monster-ascii
  (list "                  # "
        "#    ##    ##    ###"
        " #  #  #  #  #  #   "))
(define sea-monster-width (string-length (car sea-monster-ascii)))
(define sea-monster-height 3)

(define sea-monster
  (let ([sea-monster-chars (* sea-monster-height sea-monster-width)])
    (filter-map (λ (ch pos) (if (equal? ch #\#)
                                (list
                                 (quotient pos sea-monster-width)
                                 (modulo pos sea-monster-width))
                                #f))
                (string->list (apply string-append sea-monster-ascii))
                (range sea-monster-chars))))

(define (find-sea-monsters im)
  (let* ([size (add1 (apply max (map car (hash-keys im))))]
         [points (cartesian-product (range size) (range size))]
         [point-sets (map (λ (p) (map (λ (sm) (list (+ (car p) (car sm))
                                                    (+ (cadr p) (cadr sm))))
                                      sea-monster)) points)])
    (filter (λ (ps) (andmap (compose (curry equal? 1) (curryr (curry hash-ref im) #f)) ps))
            point-sets)))

(define (sea-roughness im)
  (let ([sea-monster-patches (apply set (apply append (find-sea-monsters image)))]
        [rough-water-patches (apply set (filter-map (λ (x) (if (equal? (cdr x) 1)
                                                               (car x)
                                                               #f))
                                                    (hash->list im)))])
    (set-count (set-subtract rough-water-patches sea-monster-patches))))

(displayln (sea-roughness image))
