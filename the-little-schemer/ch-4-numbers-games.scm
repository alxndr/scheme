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
