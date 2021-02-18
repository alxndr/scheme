(load "ch-1-toys.scm") ; define atom?
(load "ch-4-numbers-games.scm") ; define exponent

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp)
       (number? aexp))

      (else
        (and (numbered? (car aexp))
             (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((numbered? nexp)
       (cond
         ((atom? nexp) nexp)

         ((eq? (car (cdr nexp)) (quote +))
          (plus (car nexp) (car (cdr (cdr nexp)))))

         ((eq? (car (cdr nexp)) (quote *))
          (times (car nexp) (car (cdr (cdr nexp)))))

         ((eq? (car (cdr nexp)) (quote ^))
          (exponent (car nexp) (car (cdr (cdr nexp)))))

         (else #f))
       )
      (else #f))))
