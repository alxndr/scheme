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
