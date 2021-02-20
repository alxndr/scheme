(load "ch-2-do-it-do-it-again.scm")
(load "ch-3-cons-the-magnificent.scm")

(define set?
  (lambda (lat)
    (cond
      ((null? lat)
       #t)
      ((member? (car lat) (cdr lat))
       #f)
      (else
        (set? (cdr lat))))))

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

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1)
       set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
        (cons (car set1) (union (cdr set1) set2))))))

(define first
  (lambda (l-set)
    (car l-set)))

(define second
  (lambda (l-set)
    (car (cdr l-set))))

(define third
  (lambda (l-set)
    (car (cdr (cdr l-set)))))

(define sets-remaining
  (lambda (l-set)
    (cdr (cdr l-set))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? l-set)
       #f) ; they said this would never happen
      ((null? (sets-remaining l-set))
       (intersect (first l-set) (second l-set)))
      ((null? (second l-set))
       (first l-set))
      (else
        (intersectall (cons
                        (intersect (first l-set) (second l-set))
                        (sets-remaining l-set)))))))

(define a-pair?
  (lambda (l)
    (cond
      ((null? l)
       #f)
      ((null? (cdr l))
       #f)
      ((null? (cdr (cdr l)))
       #t)
      (else
        #f))))

(define build ; build a pair
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

; "rel" = relation, aka a set-of-pairs; set where each element is unique?? and is a 2-length list-of-atoms
; "fun" = function.......
; a finite function is a rel where the heads of all pairs are unique

(define fun?
  (lambda (rel)
    (set? (firsts rel))))
