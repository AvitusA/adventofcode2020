#lang racket

(define puzzle-input (list 1 3 5 4 6 8 7 2 9))
(define puzzle-input2 (append puzzle-input (range 10 1000001)))

(struct node (left right) #:transparent)

;; Doubly linked list in hashmap, for (nearly) constant T access/insertion
(define (make-node-hash input)
  (letrec ([helper
            (λ (xs prev acc)
              (cond [(empty? (cdr xs)) ;; Last element - return
                     (hash-set acc (car xs) (node prev (first input)))]
                    [(not prev) ;; First element
                     (helper (cdr xs)
                             (car xs)
                             (hash-set acc (car xs) (node (last input) (cadr xs))))]
                    [else
                     (helper (cdr xs)
                             (car xs)
                             (hash-set acc (car xs) (node prev (cadr xs))))]))])
    (helper input #f (hash))))

(define (navigate dir-proc node-hash node n)
  (if (equal? 1 n)
      (dir-proc (hash-ref node-hash node))
      (navigate dir-proc node-hash (dir-proc (hash-ref node-hash node)) (sub1 n))))

(define (node-hash->list node-hash first-element)
  (letrec ([helper
            (λ (xs)
              (cond [(equal? first-element (car xs)) xs]
                    [else (helper (cons (navigate node-left node-hash (car xs) 1) xs))]))])
    (helper (list (navigate node-left node-hash first-element 1)))))

(define (move node-hash current-cup n-cups)
  (let* ([picked-up-cups (list (navigate node-right node-hash current-cup 1)
                               (navigate node-right node-hash current-cup 2)
                               (navigate node-right node-hash current-cup 3))]
         [post-pickup
          ((compose
            (curryr hash-remove (first picked-up-cups))
            (curryr hash-remove (second picked-up-cups))
            (curryr hash-remove (third picked-up-cups))
            ;; Repair current cup
            (curryr hash-set current-cup
                    (node (navigate node-left node-hash current-cup 1)
                          (navigate node-right node-hash current-cup 4)))
            ;; Repair right of picked up cups
            (curryr hash-set (navigate node-right node-hash current-cup 4)
                    (node current-cup
                          (navigate node-right node-hash current-cup 5))))
           node-hash)]
         [destination (for/first ([d (in-sequences (in-range (sub1 current-cup) 0 -1)
                                                   (in-range n-cups current-cup -1))]
                                  #:when (not (member d picked-up-cups)))
                        d)]
         [post-insertion
          ((compose
            (curryr hash-set destination
                    (node (navigate node-left post-pickup destination 1)
                          (first picked-up-cups)))
            (curryr hash-set (first picked-up-cups)
                    (node destination (second picked-up-cups)))
            (curryr hash-set (second picked-up-cups)
                    (node (first picked-up-cups) (third picked-up-cups)))
            (curryr hash-set (third picked-up-cups)
                    (node (second picked-up-cups)
                          (navigate node-right post-pickup destination 1)))
            (curryr hash-set (navigate node-right post-pickup destination 1)
                    (node (third picked-up-cups)
                          (navigate node-right post-pickup destination 2))))
           post-pickup)]
         [next-current (navigate node-right post-insertion current-cup 1)])
    (values post-insertion next-current)))

(define (play cups current-cup n n-cups)
  (let-values ([(result-cups next-current-cup) (move cups current-cup n-cups)])
    (if (n . > . 1)
        (play result-cups next-current-cup (sub1 n) n-cups)
        result-cups)))

(define (crabstring node-hash)
  (string-join (map number->string (drop (node-hash->list node-hash 1) 1)) ""))

(define (secret-product node-hash)
  (* (navigate node-right node-hash 1 1)
     (navigate node-right node-hash 1 2)))

;; Part 1
(displayln (crabstring (play (make-node-hash puzzle-input)
                             (car puzzle-input)
                             100
                             (length puzzle-input))))

;; Part 2 - Takes about 5 min to run on i9-9900K. Could optimize
;; a bit but won't bother
(displayln (secret-product (play (make-node-hash puzzle-input2)
                                 (car puzzle-input2)
                                 10000000
                                 (length puzzle-input2))))

