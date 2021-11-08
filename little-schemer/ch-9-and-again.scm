(define keep-looking
  (lambda (atm symbol-or-number list-of-atoms)
    (cond
      ((number? symbol-or-number)
       (keep-looking atm (pick symbol-or-number list-of-atoms) list-of-atoms))
      (else
        (eq? atm symbol-or-number)))))

(define looking
  (lambda (atm list-of-atoms)
    (keep-looking atm (pick 1 list-of-atoms) list-of-atoms)))

(define shift
  (lambda (pair-of-tups)
    (cons
      (car (car pair-of-tups))
      (cons
        (cons
          (car (cdr (car pair-of-tups)))
          (cdr pair-of-tups))
        '()))))

(define align
  ; dependencies aren't defined
  (lambda (pair-or-atom)
    (cond
      ((atom? pair-or-atom)
       pair-or-atom)
      ((a-pair? (first pair-or-atom))
       (align (shift pair-or-atom)))
      (else
        (build (first pair-or-atom)
               (align (second pair-or-atom)))))))

(define length*
  (lambda (pair-or-atom)
    (cond
      ((null? pair-or-atom)
       0)
      ((atom? pair-or-atom)
       1)
      (else
       (plus (length* (car pair-or-atom))
             (length* (cdr pair-or-atom)))))))

(define weight*
  (lambda (pair-or-atom)
    (cond
      ((atom? pair-or-atom) 1)
      (else
        (plus
          (times
            (weight* (first pair-or-atom)) 2)
          (weight* (second pair-or-atom)))))))

(define shuffle
  (lambda (pair-or-atom)
    (cond
      ((atom? pair-or-atom) pair-or-atom)
      ((a-pair? (first pair-or-atom))
       (shuffle (revpair pair-or-atom)))
      (else
        (build (first pair-or-atom)
               (shuffle (second pair-or-atom)))))))

(define C ; Lothar Collatz
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (div n 2)))
      (else
        (C (add1 (times 3 n)))))))

(define A ; Wilhelm Ackermann
  (lambda (x y)
    (cond
      ((zero? x) (add1 y))
      ((zero? y) (A (sub1 x) 1))
      (else
        (A (sub1 x)
           (A x (sub1 y)))))))

((lambda (Y)
   (Y Y))
 (lambda (Y)
   ((lambda (len)
      (lambda (param)
        (cond
          ((null? param) 0)
          (else
            (add1 (len (cdr param)))))))
    (lambda (x) ((Y Y) x)))))
