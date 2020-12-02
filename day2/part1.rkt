#lang racket
(require racket/match)

(define puzzle-input (string-split (port->string) "\n"))

(define (match-case str)
  (cdr (regexp-match #px"(\\d+)-(\\d+) (\\w): (\\w+)" str)))

(define (parse-and-validate-case str validator)
  (match (match-case str)
    [(list lower upper char password)
     (validator (string->number lower) (string->number upper) char password)]))

(define (valid-password1? lower upper char password)
  (let ([n (length (regexp-match* (regexp char) password))])
       (and (lower . <= . n)
            (n . <= . upper))))

(define (valid-password2? pos1 pos2 char password)
  (let ([matching-char (string-ref char 0)]
        [char1 (string-ref password (pos1 . - . 1))]
        [char2 (string-ref password (pos2 . - . 1))])
    (and (not (equal? char1 char2))
         (or (equal? char1 matching-char)
             (equal? char2 matching-char)))))

(displayln (length (filter (lambda (str) (parse-and-validate-case
                                          str
                                          valid-password2?))
                           puzzle-input)))
