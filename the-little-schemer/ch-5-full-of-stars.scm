(define atom?
  (lambda (x)
    (and
      (not (pair? x))
      (not (null? x)))))

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      (( or (number? a) (number? b)) #f)
      (else
        (eq? a b)
      )
    )
  )
)

(define rember*
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((atom? (car lat))
        (cond
          ((eqan? a (car lat)) (rember* a (cdr lat)))
          (else
            (cons
              (car lat)
              (rember* a (cdr lat))
            )
          )
        )
      )
      (else
        (cons
          (rember* a (car lat))
          (rember* a (cdr lat))
        )
      )
    )
  )
)

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f)
    )
  )
)

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ( (eqan? old (car l))
        (cons
          (car l)
          (cons new (cdr l))
        )
      )
      (else
        (cons
          (car l)
          (insertR* new old (cdr l))
        )
      )
    )
  )
)

