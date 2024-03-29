(load "./ch-4-numbers-games.scm")

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
              (else
                (and
                  (equal? (car l1) (car l2))
                  (equal? (cdr l1) (cdr l2)))))))

(define
  equal? (lambda (s1 s2)
           (cond
             ((and
                (atom? s1)
                (atom? s2))
              (eqan? s1 s2))
             ((or
                (atom? s1)
                (atom? s2))
              #f)
             (else
               (eqlist? s1 s2)))))

(define rember
  (lambda (needle haystack) ; needle is any S-expression, haystack is list of S-expressions
    (cond
      ((null? haystack)
       '())
      ((equal? (car haystack) needle)
       (cdr haystack))
    (else
      (cons (car haystack)
            (rember needle (cdr haystack)))))))
