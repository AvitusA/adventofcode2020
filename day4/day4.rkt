#lang racket

(define (get-fields str)
  (map (lambda(x) (string-split x ":"))
       (regexp-match* #px"(byr|iyr|eyr|hgt|hcl|ecl|pid):(\\w|#)+" str)))

(define (str-range str lower upper)
  (let ([number (string->number str)])
    (and (lower . <= . number) (number . <= . upper))))

(define (valid-field-value? field-pair)
  (match field-pair
    [(list "byr" (pregexp #px"^\\d{4}$" (cons byr _))) (str-range byr 1920 2002)]
    [(list "iyr" (pregexp #px"^\\d{4}$" (cons iyr _))) (str-range iyr 2010 2020)]
    [(list "eyr" (pregexp #px"^\\d{4}$" (cons eyr _))) (str-range eyr 2020 2030)]
    [(list "hgt" (pregexp #px"^\\d{2}(?=in$)" (cons hgt _))) (str-range hgt 59 76)]
    [(list "hgt" (pregexp #px"^\\d{3}(?=cm$)" (cons hgt _))) (str-range hgt 150 193)]
    [(list "hcl" (pregexp #px"^#[0-9a-f]{6}$")) #t]
    [(list "ecl" (pregexp #px"^(amb|blu|brn|gry|grn|hzl|oth)$")) #t]
    [(list "pid" (pregexp #px"^\\d{9}$")) #t]
    [_ #f]))

(define (contains-required-fields? fields) (equal? 7 (length fields)))

(define (contains-only-valid-fields? fields) (andmap valid-field-value? fields))

(define passports (map get-fields (string-split (port->string) "\n\n")))
(displayln (count contains-required-fields? passports)) ;; Part 1
(displayln (count (conjoin contains-required-fields? contains-only-valid-fields?)
                  passports)) ;; Part 2
