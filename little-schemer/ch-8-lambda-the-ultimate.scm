(define rember-f
  (lambda (f a l)
    (cond
      ((null? l)
       '())
      ((apply f (quasiquote (a (unquote (car l)))))
       (cdr l))
      (else
        (cons (car l)
              (rember-f f a (cdr l)))))))
