(define atom?
  (lambda (x)
    (and
      (not (pair? x))
      (not (null? x))
    )
  )
)

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

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
        (+ 1 (length (cdr l)))
      )
    )
  )
)

