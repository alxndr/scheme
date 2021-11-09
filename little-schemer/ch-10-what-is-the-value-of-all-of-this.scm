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

(define extend-table
 ; "…takes an entry and a table and … put[s] the new entry in front of the old table"
 cons)

(define lookup-in-table
  (lambda (name table fn-not-found)
    (cond
      ((null? table) (fn-not-found name))
      (else
        (lookup-in-entry name
                         (car table)
                         (lambda (name)
                           (lookup-in-table name
                                            (cdr table)
                                            fn-not-found)))))))

(define build
  (lambda (t v)
    (cons t (cons v '()))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build 'primitive e)))))

(define atom-to-action
  (lambda (atm)
    (cond
      ((number? atm) *const)
      ((eq? atm #t) *const)
      ((eq? atm #f) *const)
      ((eq? atm 'cons) *const)
      ((eq? atm 'car) *const)
      ((eq? atm 'cdr) *const)
      ((eq? atm 'null?) *const)
      ((eq? atm 'eq?) *const)
      ((eq? atm 'atom?) *const)
      ((eq? atm 'zero?) *const)
      ((eq? atm 'add1) *const)
      ((eq? atm 'sub1) *const)
      ((eq? atm 'number?) *const)
      (else *identifier))))

(define text-of
  second)

(define *quote
  (lambda (e table)
    (text-of e)))

(define *identifier
  (lambda (e table)
    (lookup-in-table e
                     table
                     (lambda (name) 'NOTFOUND))))

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define value
  ; "…approximates the function `eval` in Scheme"
  (lambda (e)
    (meaning e '())))

(define table-of
  first)

(define formals-of
  second)

(define body-of
  third)

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of
  first)

(define answer-of
  second)

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))

(define cond-lines-of
  cdr)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))