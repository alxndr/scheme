(define rember-f
  (lambda (f a l)
    (cond
      ((null? l)
       '())
      ((f (car l) a)
       (cdr l))
      (else
        (cons (car l)
              (rember-f f a (cdr l)))))))
