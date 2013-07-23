(define add1
  (lambda (n)
    (+ n 1)
  )
)

(define sub1
  (lambda (n)
    (- n 1)
  )
)

(define o+
  (lambda (a b)
    (cond
      ((zero? a) b)
      ((zero? b) a)
      (else
        (o+ (add1 a) (sub1 b))
      )
    )
  )
)

(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
        (o- (sub1 b) (sub1 a))
      )
    )
  )
)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) '() )
      ((zero? (car tup)) 0)
      (else
        (addtup 
          (cons 
            (- (car tup) 1) 
            (cons 
              (+ (car (cdr tup) 1) 
                 (cdr (cdr tup))
               )
            )
          )
        )
      )
    )
  )
)

