(load "ch-2-do-it-do-it-again.scm")
(load "ch-3-cons-the-magnificent.scm")

(define set?-helper
  (lambda (lat seen)
    (cond
      ((null? lat)
       #t)
      ((member? seen (car lat))
       #f)
      (else
        (set?-helper (cdr lat) (cons seen (car lat)))))))

(define set?
  (lambda (lat)
    (set?-helper lat '())))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat)
       '())
      (else
        (cons (car lat) (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (or
      (null? set1)
      (and
        (member? (car set1) set2)
        (subset? (cdr set1) set2)))))

(define eqset?
  (lambda (set1 set2)
    (or
      (and (null? set1) (null? set2))
      (and
        (eq? (car set1) (car set2))
        (eqset? (cdr set1) (cdr set2)))
      )))
