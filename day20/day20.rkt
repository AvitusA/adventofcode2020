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

(define (symmetry-group tile)
  (map (λ (x) (x tile))
       (map (curry apply compose) (cartesian-product
                                   (list identity hflip)
                                   (list identity rot90
                                         (compose rot90 rot90)
                                         (compose rot90 rot90 rot90))))))

(define puzzle-input (with-input-from-file "input" port->string))
(define tiles (map (compose symmetry-group parse-tile) (string-split puzzle-input "\n\n")))

;; (define puzzle-input
;; "Tile 2311:
;; ..##.#..#.
;; ##..#.....
;; #...##..#.
;; ####.#...#
;; ##.##.###.
;; ##...#.###
;; .#.#.#..##
;; ..#....#..
;; ###...#.#.
;; ..###..###

;; Tile 1951:
;; #.##...##.
;; #.####...#
;; .....#..##
;; #...######
;; .##.#....#
;; .###.#####
;; ###.##.##.
;; .###....#.
;; ..#.#..#.#
;; #...##.#..

;; Tile 1171:
;; ####...##.
;; #..##.#..#
;; ##.#..#.#.
;; .###.####.
;; ..###.####
;; .##....##.
;; .#...####.
;; #.##.####.
;; ####..#...
;; .....##...

;; Tile 1427:
;; ###.##.#..
;; .#..#.##..
;; .#.##.#..#
;; #.#.#.##.#
;; ....#...##
;; ...##..##.
;; ...#.#####
;; .#.####.#.
;; ..#..###.#
;; ..##.#..#.

;; Tile 1489:
;; ##.#.#....
;; ..##...#..
;; .##..##...
;; ..#...#...
;; #####...#.
;; #..#.#.#.#
;; ...#.#.#..
;; ##.#...##.
;; ..##.##.##
;; ###.##.#..

;; Tile 2473:
;; #....####.
;; #..#.##...
;; #.##..#...
;; ######.#.#
;; .#...#.#.#
;; .#########
;; .###.#..#.
;; ########.#
;; ##...##.#.
;; ..###.#.#.

;; Tile 2971:
;; ..#.#....#
;; #...###...
;; #.#.###...
;; ##.##..#..
;; .#####..##
;; .#..####.#
;; #..#.#..#.
;; ..####.###
;; ..#.#.###.
;; ...#.#.#.#

;; Tile 2729:
;; ...#.#.#.#
;; ####.#....
;; ..#.#.....
;; ....#..#.#
;; .##..##.#.
;; .#.####...
;; ####.#.#..
;; ##.####...
;; ##..#.##..
;; #.##...##.

;; Tile 3079:
;; #.#.#####.
;; .#..######
;; ..#.......
;; ######....
;; ####.#..#.
;; .#...#.##.
;; #.#####.##
;; ..#.###...
;; ..#.......
;; ..#.###...")
