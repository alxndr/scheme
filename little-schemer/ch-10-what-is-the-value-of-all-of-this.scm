; "An entry is a pair of lists whose first list is a set.
; "…the two lists must be of equal length."

(define lookup-in-entry
  (lambda (name entry fn-not-found)
    ; find atom in 2nd list which matches the position of name in 1st list
    ; if not found, call fn-not-found with name
    (lookup-in-entry-helper name
                            (first entry)
                            (second entry)
                            fn-not-found)))

(define lookup-in-entry-helper
  (lambda (name names vals fn-not-found)
    (cond
      ((null? names) (fn-not-found name))
      ((eq? name (car names))
       (car vals))
      (else
        (lookup-in-entry-helper name
                                (cdr names)
                                (cdr vals)
                                fn-not-found)))))

; "A table [aka environment] is a list of entries."

;(define extend-table
;  ; "…takes an entry and a table and … put[s] the new entry in front of the old table"
;  (lambda (entry table)
;    null))

(define lookup-in-table
  (lambda (name table fn-not-found)
    (cond
      ((null? table) (fn-not-found name))
      (else
        (lookup-in-entry name
                         (car table)
                         (lambda (name) (lookup-in-table name (cdr table) fn-not-found)))))))
