#lang racket
(require racket/match)
(require racket/list)

(define puzzle-input (string-split (port->string) "\n"))

(struct password-case (lower upper char password))

(define (parse-case str)
  (match (cdr (regexp-match #px"(\\d+)-(\\d+) (\\w): (\\w+)" str))
    [(list lower-str upper-str char password)
     (password-case (string->number lower-str)
                    (string->number upper-str)
                    (string-ref char 0)
                    password)]))

(define (valid-password1? the-case)
  (match-let* ([(password-case lower upper char password) the-case]
               [n (count (lambda (x) (equal? x char)) (string->list password))])
    (and (lower . <= . n)
         (n . <= . upper))))

(define (valid-password2? the-case)
  (match-let* ([(password-case pos1 pos2 char password) the-case]
               [char1 (string-ref password (pos1 . - . 1))]
               [char2 (string-ref password (pos2 . - . 1))])
    (and (not (equal? char1 char2))
         (or (equal? char1 char)
             (equal? char2 char)))))

(displayln (count valid-password1? (map parse-case puzzle-input)))
