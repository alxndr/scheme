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
    (and
      (subset? set1 set2)
      (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (and
      (not (null? set1))
      (or
        (member? (car set1) set2)
        (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1)
       '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2))
      )))
