#lang racket

;; Part 1
(define (parse-ingredient-list str)
  (let ([ingredients ((compose
                       (curryr string-split " ")
                       car
                       (curry regexp-match #px"^((\\w+) )+(?=\\()")) str)]
        [allergens ((compose
                      (curryr string-split ", ")
                      car
                      (curry regexp-match #px"(?<=\\(contains )[\\w, ]*(?=\\))")) str)])
    (map (curryr cons (apply set ingredients)) allergens)))

(define (allergen-clauses ingredient-lists)
  (let* ([all-clauses (apply append ingredient-lists)]
         [all-allergens (remove-duplicates (map car all-clauses))])
    (map (λ (a) (cons a (filter-map (λ (c) (if (equal? (car c) a) (cdr c) #f)) all-clauses))) all-allergens)))

(define (intersect-clauses clauses)
  (map (λ (conjunction) (list (car conjunction)
                              (foldr set-intersect
                                     (apply set-union (cdr conjunction))
                                     (cdr conjunction)))) clauses))

(define (impossible-ingredient-occurrence str)
  (let* ([ingredient-lists (map parse-ingredient-list (string-split str "\n"))]
         [ingredients-in-lists (map (compose cdr car) ingredient-lists)]
         [clauses (allergen-clauses ingredient-lists)]
         [all-ingredients (apply set-union (apply append (map cdr clauses)))]
         [pruned-clauses (intersect-clauses clauses)]
         [remaining-ingredients (apply set-union (apply append (map cdr pruned-clauses)))]
         [impossible-ingredients (set-subtract all-ingredients remaining-ingredients)])
    (foldr (λ (x acc) (+ acc (count (λ (l) (set-member? l x)) ingredients-in-lists))) 0
           (set->list impossible-ingredients))))

;; Part 2
(define (parse-clauses str)
  (allergen-clauses (map parse-ingredient-list (string-split str "\n"))))

(define (alldifferent clauses)
  (let* ([unit-clauses (filter (compose (curry equal? 1) set-count cadr) clauses)]
         [post-propagation
          (map (λ (c)
                 (foldr (λ (x acc) (if (equal? (car c) (car x))
                                       acc ;; Don't remove from self
                                       (list (car c) (set-subtract (cadr acc) (cadr x)))))
                        c
                        unit-clauses)) clauses)])
    (cond
      [(equal? post-propagation clauses) post-propagation]
      [(ormap (compose set-empty? cadr) post-propagation) #f] ;; Blowout
      [else (alldifferent post-propagation)])))

(define (dangerous-list str)
  (let* ([allergens (alldifferent (intersect-clauses (parse-clauses str)))]
         [sorted-allergens (sort allergens (λ (lhs rhs) (string<? (car lhs) (car rhs))))])
    (string-join (map (compose car set->list cadr) sorted-allergens) ",")))

;; Ouput
(define puzzle-input (port->string))
(displayln (impossible-ingredient-occurrence puzzle-input))
(displayln (dangerous-list puzzle-input))
