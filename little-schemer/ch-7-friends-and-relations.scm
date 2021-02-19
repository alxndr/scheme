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
        (cons (car lat) (multirember (car lat) (makeset (cdr lat))))))))
