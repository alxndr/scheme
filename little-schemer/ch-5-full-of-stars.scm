(define rember* (lambda (a l)
                  (cond
                    ((null? l)
                     '())
                    ((and (atom? (car l)) (eqan? (car l) a))
                     (rember* a (cdr l)))
                    ((atom? (car l))
                     (cons (car l) (rember* a (cdr l))))
                    (else
                      (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR* (lambda (new old l)
                   (cond
                     ((null? l) '())
                     ((and (atom? (car l)) (eqan? (car l) old))
                      (cons old (cons new (insertR* new old (cdr l)))))
                     ((atom? (car l))
                      (cons (car l) (insertR* new old (cdr l))))
                     (else
                       (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
