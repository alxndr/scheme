(define keep-looking
  (lambda (atm symbol-or-number list-of-atoms)
    (cond
      ((number? symbol-or-number)
       (keep-looking atm (pick symbol-or-number list-of-atoms) list-of-atoms))
      (else
        (eq? atm symbol-or-number)))))

(define looking
  (lambda (atm list-of-atoms)
    (keep-looking atm (pick 1 list-of-atoms) list-of-atoms)))
