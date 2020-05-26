(define replace
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons
              (car lat)
              (replace new old (cdr lat))
      ))
    )
  )
)

