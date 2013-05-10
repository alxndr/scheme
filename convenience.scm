(define first
  (lambda (l)
    (cond
      ((null? l) '())
      (else
        (car l)
      )
    )
  )
)

(define second
  (lambda (l)
    (cond
      ((null? (first l)) '())
      (else
        (first (cdr l))
      )
    )
  )
)

(define last
  (lambda (l)
    (cond
      ((null? l) '())
      ((null? (cdr l)) (car l))
      (else
       (last (cdr l))
      )
    )
  )
)

