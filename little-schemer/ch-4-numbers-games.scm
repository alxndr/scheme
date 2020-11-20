(define add1 (lambda (n)
               (+ n 1)))
(define sub1 (lambda (n)
               (- n 1)))

(define plus (lambda (a b)
               (cond
                 ((zero? a) b)
                 (else
                   (plus (sub1 a) (add1 b))))))
                   ; (add1 (plus a (sub1 b))))))) ; less efficient??

(define minus (lambda (a b)
                (cond
                  ((zero? b) a)
                  (else
                    (minus (sub1 a) (sub1 b))))))

(define addtup (lambda (tup)
                 (cond
                   ((null? tup) 0)
                   (else
                     (plus (car tup) (addtup (cdr tup)))))))

(define times (lambda (a b)
                (cond
                  ((zero? a) 0)
                  ; ((zero? b) 0)
                  ; ((zero? (sub1 a)) b)
                  ; ((zero? (sub1 b)) a)
                  (else (plus b (times (sub1 a) b))))))

(define tup+ (lambda (tup1 tup2)
               (cond
                 ((and (null? tup1) (null? tup2))
                  '())
                 ((null? tup1)
                  (cons (car tup2) (tup+ '() (cdr tup2) )))
                 ((null? tup2)
                  (cons (car tup1) (tup+ (cdr tup1) '() )))
                 (else
                   (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
