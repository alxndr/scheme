(define numbered?
  (lambda (aexp)
    (cond
      ((atom? (car aexp))
       ; if car aexp is a number, then: next val must be null-or-operator,  and val after must be number-or-list
       ; if car aexp is operator.......... #f ?? cause invalid syntax............
       )
      (else
        ()
        ))))
