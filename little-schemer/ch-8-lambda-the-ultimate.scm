(define rember-f
  (lambda (test-fun atm lst)
    (cond
      ((null? lst)
       '())
      ((test-fun (car lst) atm)
       (cdr lst))
      (else
        (cons (car lst)
              (rember-f test-fun atm (cdr lst)))))))

(define eq?-curry
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-curry 'salad))

(define rember-f-curry
  (lambda (test-fun)
    (lambda (atm lst)
      (cond
        ((null? lst)
         '())
        ((test-fun (car lst) atm)
         (cdr lst))
        (else
          (cons (car lst)
                (rember-f test-fun atm (cdr lst))))))))
