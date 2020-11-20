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
                 ((null? tup1) tup2)
                 ((null? tup2) tup1)
                 (else
                   (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define > (lambda (a b)
            (cond
              ((zero? a) #f)
              ((zero? b) #t)
              (else (> (sub1 a) (sub1 b))))))

(define < (lambda (a b)
            (cond
              ((zero? b) #f)
              ((zero? a) #t)
              (else (< (sub1 a) (sub1 b))))))

(define = (lambda (a b)
            (and (not (< a b)) (not (> a b)))))

(define ^ (lambda (base power)
            (cond
              ((zero? power) 1)
              (else (times base (^ base (sub1 power)))))))

(define div (lambda (numerator denominator)
              (cond
                ((< numerator denominator) 0)
                (else (add1 (div (minus numerator denominator) denominator))))))

(define length (lambda (lat)
                 (cond
                   ((null? lat) 0)
                   (else (add1 (length (cdr lat)))))))

(define pick (lambda (n lat)
               (cond
                 ((zero? n) '())
                 ((null? lat) '())
                 ((zero? (sub1 n)) (car lat))
                 (else (pick (sub1 n) (cdr lat))))))

(define rempick (lambda (n lat)
                  (cond
                    ((null? lat) '())
                    ((zero? (sub1 n)) (cdr lat))
                    (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums (lambda (lat)
                  (cond
                    ((null? lat) '())
                    ((number? (car lat)) (no-nums (cdr lat)))
                    (else (cons (car lat) (no-nums (cdr lat)))))))
