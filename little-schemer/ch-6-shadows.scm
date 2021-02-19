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

(define valueInfix
  (lambda (nexp)
    (cond
      ((number? nexp)
       nexp)
      ((eq? (quote +) (car (cdr nexp)))
       (plus (value (car nexp))
             (value (car (cdr (cdr nexp))))))
      ((eq? (quote *) (car (cdr nexp)))
       (times (value (car nexp))
              (value (car (cdr (cdr nexp))))))
      ((eq? (quote ^) (car (cdr nexp)))
       (exponent (value (car nexp))
                 (value (car (cdr (cdr nexp))))))
      (else #f))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (lambda (nexp)
    (cond
      ((number? nexp)
       nexp)
      ((eq? '+ (car nexp))
       (plus (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp))))
      ((eq? '* (car nexp))
       (times (value (1st-sub-exp nexp))
              (value (2nd-sub-exp nexp))))
      ((eq? '^ (car nexp))
       (exponent (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))
