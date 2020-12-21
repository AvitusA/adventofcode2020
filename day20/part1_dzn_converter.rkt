#lang racket

(define (parse-tile tile-str)
  (let* ([lines (string-split tile-str "\n")]
         [title-str (first lines)]
         [pixels-str (map (compose (curryr string-replace "." "0")
                                   (curryr string-replace "#" "1")) (cdr lines))])
    (cons (string->number (last (regexp-match #px"(?:Tile (\\d+):$)" title-str)))
          (map (compose (curry map (compose string->number string)) string->list) pixels-str))))

(define (hflip tile)
  (cons (first tile)
        (map reverse (cdr tile))))

(define (height tile) (sub1 (length tile)))
(define (width tile) (length (second tile)))

(define (rot90 tile)
  (let ([in-width (width tile)]
        [in-height (height tile)]
        [in-pixels (cdr tile)])
    (cons (car tile)
          (for/list ([out-row (in-range in-width)])
            (for/list ([out-col (in-range in-height)])
              (list-ref 
               (list-ref in-pixels (sub1 (- in-height out-col)))
               out-row))))))

(define (group tile)
  (map (λ (x) (x tile))
       (map (curry apply compose) (cartesian-product
                                   (list identity hflip)
                                   (list identity rot90
                                         (compose rot90 rot90)
                                         (compose rot90 rot90 rot90))))))

(define (part1)
  (define puzzle-input (port->string))
  (define tiles (map (compose group parse-tile) (string-split puzzle-input "\n\n")))
  (printf "puzzle_size = ~a;~n" (sqrt (length (map (compose car car) tiles))))
  (printf "tile_size = ~a;~n" (width (car (car tiles))))
  (printf "n_tiles = ~a;~n" (length (apply append tiles)))
  (printf "tile_ids = { ~a };~n" ((compose 
                                   (curryr string-join ", ")
                                   remove-duplicates
                                   (curry map (compose number->string car)))
                                  (apply append tiles)))
  (printf "tile_id = [ ~a ];~n" ((compose (curryr string-join ", ")
                                          (curry map (compose number->string car)))
                                 (apply append tiles)))
  (printf "tile = array3d(1..~a, 1..~a, 1..~a, [ ~a ]);~n"
          (length (apply append tiles))
          (width (car (car tiles)))
          (width (car (car tiles)))
          (string-join (map number->string (flatten (map cdr (apply append tiles)))) ", ")))

;; Part 1 (format as .dzn for minizinc)
(part1)
