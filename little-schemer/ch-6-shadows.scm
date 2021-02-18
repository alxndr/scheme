(load "ch-1-toys.scm") ; define atom?
(load "ch-4-numbers-games.scm") ; define exponent

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)
       (number? aexp))

      ((eq? (car (cdr aexp)) (quote +))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote *))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^))
       (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))

      (else
        (#f)))))

(define value
  (lambda (aexp)
    (cond
      ((numbered? aexp)
       (cond
         ((atom? aexp) aexp)

         ((eq? (car (cdr aexp)) (quote +))
          (plus (car aexp) (car (cdr (cdr aexp)))))

         ((eq? (car (cdr aexp)) (quote *))
          (times (car aexp) (car (cdr (cdr aexp)))))

         ((eq? (car (cdr aexp)) (quote ^))
          (exponent (car aexp) (car (cdr (cdr aexp)))))

         (else #f))
       )
      (else #f))))
