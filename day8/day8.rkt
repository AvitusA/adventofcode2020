#lang racket

(struct machine-state (imem imem-size ip acc) #:transparent)

(define (machine-halted? state) ;; ip advanced beyond imem
  (>= (machine-state-ip state) (machine-state-imem-size state)))

(define (parse-instruction line)
  (match (string-split line " ")
    [(list operation operand) (list (string->symbol operation)
                                    (string->number operand))]))

(define (load-code lines)
  (make-immutable-hash
   (for/list ([addr (in-naturals)]
              [instr (map parse-instruction (string-split lines "\n"))])
     (cons addr instr))))

(define (boot-machine code)
  (let* ([imem (load-code code)]
         [imem-size (apply max (hash-keys imem))])
    (machine-state imem imem-size 0 0)))

(define (tick state)
  (match (hash-ref (machine-state-imem state) (machine-state-ip state))
    [(list 'nop _)
     (struct-copy machine-state state
                  [ip (+ 1 (machine-state-ip state))])]
    [(list 'jmp operand)
     (struct-copy machine-state state               
                  [ip (+ operand (machine-state-ip state))])]    
    [(list 'acc operand)
     (struct-copy machine-state state
                  [acc (+ operand (machine-state-acc state))]
                  [ip (+ 1 (machine-state-ip state))])]))

(define (advance-to-first-repetition-or-halt state executed-instructions)
  (cond
    [(machine-halted? state) state]
    [(set-member? executed-instructions (machine-state-ip state)) state]
    [else (advance-to-first-repetition-or-halt (tick state)
                                               (set-add executed-instructions
                                                        (machine-state-ip state)))]))

(define initial-state (boot-machine (port->string)))

;; Part 1
(displayln (machine-state-acc (advance-to-first-repetition-or-halt initial-state (set))))

;; Part 2
(define (patch state address)
  (match (hash-ref (machine-state-imem state) address)
    [(list 'acc _) state]
    [(list 'jmp operand)
     (struct-copy machine-state state
                  [imem (hash-set (machine-state-imem state) address (list 'nop operand))])]
    [(list 'nop operand)
     (struct-copy machine-state state
                  [imem (hash-set (machine-state-imem state) address (list 'jmp operand))])]))

(displayln
 (for/first ([address (in-range 0 (machine-state-imem-size initial-state))]
             #:when (machine-halted?
                     (advance-to-first-repetition-or-halt
                      (patch initial-state address) (set))))
   (machine-state-acc (advance-to-first-repetition-or-halt
                       (patch initial-state address) (set)))))
