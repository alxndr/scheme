#lang scheme

(define add1
  (lambda (n)
    (+ n 1)
  )
)

(define sub1
  (lambda (n)
    (- n 1)
  )
)

(define o+
  (lambda (a b)
    (cond
      ((zero? a) b)
      ((zero? b) a)
      (else
        (o+ (add1 a) (sub1 b))
      )
    )
  )
)

(define o-
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
        (o- (sub1 b) (sub1 a))
      )
    )
  )
)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 
       0
      )
      ((zero? (car tup))
       (cdr tup)
      )
      (else
        (o+
          (car tup)
          (addtup (cdr tup))
        )
      )
    )
  )
)

(define x
  (lambda (a b)
    (cond
      ((zero? a) 0)
      ((zero? b) 0)
      ((zero? (sub1 a)) b)
      ((zero? (sub1 b)) a)
      (else
        (o+
          a
          (x a (sub1 b))
        )
      )
    )
  )
)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2))
        '()
      )
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons
          (o+ (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2))
        )
      )
    )
  )
)

(define >
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else
        (> (sub1 a) (sub1 b))
      )
    )
  )
)

(define <
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else
        (< (sub1 a) (sub1 b))
      )
    )
  )
)

(define ==
  (lambda (a b)
    (cond
      ((< a b) #f)
      ((> a b) #f)
      (else #t)
    )
  )
)

(define ^
  (lambda (n pow)
    (cond
      ((zero? n) 0)
      ((zero? pow) 1)
      ((zero? (sub1 n)) 1)
      ((zero? (sub1 pow)) n)
      (else (x n (^ n (sub1 pow))))
    )
  )
)

(define ÷
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else
        (add1 (÷ (o- a b) b))
      )
    )
  )
)

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else
        (add1 (length (cdr lat)))
      )
    )
  )
)

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else
        (pick (sub1 n) (cdr lat))
      )
    )
  )
)

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else
        (cons
          (car lat)
          (rempick (sub1 n) (cdr lat))
        )
      )
    )
  )
)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
        (cons
          (car lat)
          (no-nums (cdr lat))
        )
      )
    )
  )
)

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) 
        (cons (car lat) (all-nums (cdr lat)))
      )
      (else
        (all-nums (cdr lat))
      )
    )
  )
)

(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      (( or (number? a) (number? b)) #f)
      (else
        (eq? a b)
      )
    )
  )
)

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else
        (occur a (cdr lat))
      )
    )
  )
)

(define one?
  (lambda (a)
    (eqan? a 1)
  )
)

(define rempick-b
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else
        (cons
          (car lat)
          (rempick (sub1 n) (cdr lat))
        )
      )
    )
  )
)
