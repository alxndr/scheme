(l "ch-4-numbers-games.scm")

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

(define occur* (lambda (a l)
                 (cond
                   ((null? l)       ; l is empty...
                                         0)
                   ((atom? (car l)) ; l is a value...
                    (cond
                      ((eq? a (car l))   (add1 (occur* a (cdr l))))
                      (else              (occur* a (cdr l)))))
                   (else            ; l is a list...
                     (plus               (occur* a (car l)) (occur* a (cdr l)))))))

(define list? (lambda (x) (not (atom? x))))
(define subst* (lambda (new old l)
                 (cond
                   ((null? l)
                    '())
                   ((list? (car l))
                    (cons (subst* new old (car l)) (subst* new old (cdr l))))
                   ((eq? old (car l))
                    (cons new (subst* new old (cdr l))))
                   (else
                     (cons (car l) (subst* new old (cdr l)))))))

(define insertL* (lambda (new old l)
                   (cond
                     ((null? l) '())
                     ((atom? (car l)) (cond
                                        ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
                                        (else (cons (car l) (insertL* new old (cdr l))))))
                     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member* (lambda (a l)
                  (cond
                    ((null? l) #f)
                    ((atom? (car l)) (cond
                                       ((eq? a (car l)) #t)
                                       (else (member* a (cdr l)))))
                    (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost (lambda (l)
                   (cond
                     ((null? l) '())
                     ((atom? (car l)) (car l))
                     (else (leftmost (car l))))))

(define
  eqlist? (lambda (l1 l2)
            (cond
              ((and
                 (null? l1)
                 (null? l2))
               #t)
              ((or
                 (null? l1)
                 (null? l2))
               #f)
              ((and
                 (atom? (car l1))
                 (atom? (car l2)))
               (and
                 (eqan?   (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2))))
              ((or
                 (atom? (car l1))
                 (atom? (car l2)))
               #f)
              (else
                (and
                  (eqlist? (car l1) (car l2))
                  (eqlist? (cdr l1) (cdr l2)))))))

(define
  equal? (lambda (a b)
           (cond
             ((and
                (null? a)
                (null? b))
              #t)
             ((and
                (atom? a)
                (atom? b))
              (eqan? a b))
             ((or
                (atom? (car a))
                (atom? (car b)))
              #f)
             (else
               (and
                 (equal? (car a) (car b))
                 (equal? (cdr a) (cdr b)))))))
