(load "ch-2-do-it-do-it-again.scm")

(define set?
  (lambda (lat)
    (cond
      ((null? lat)
       #t)

      ((member? (car lat) (cdr lat))
       #f)

      (else
        (set? (cdr lat))))))
