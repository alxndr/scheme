(define rember-f
  (lambda (test-fun atm lst)
    (cond
      ((null? lst)
       '())
      ((test-fun (car lst) atm)
       (cdr lst))
      (else
        (cons (car lst)
              (rember-f test-fun atm (cdr lst)))))))

(define eq?-curry
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-curry 'salad))

(define rember-f-curry
  (lambda (test-fun)
    (lambda (atm lst)
      (cond
        ((null? lst)
         '())
        ((test-fun (car lst) atm)
         (cdr lst))
        (else
          (cons (car lst)
                (rember-f test-fun atm (cdr lst))))))))

(define insertL-curry
  (lambda (new-value)
    (lambda (needle haystack)
      (cond
        ((null? haystack)
         '())
        ((eq? (car haystack) needle)
         (cons new-value haystack))
        (else
          (cons
            (car haystack)
            ((insertL-curry new-value) needle (cdr haystack))))))))

(define insertR-curry
  (lambda (new-value)
    (lambda (needle haystack)
      (cond
        ((null? haystack)
         '())
        ((eq? (car haystack) needle)
         (cons needle (cons new-value (cdr haystack))))
        (else
          (cons
            (car haystack)
            ((insertR-curry new-value) needle (cdr haystack))))))))

(define seqL
  (lambda (a b lst)
    (cons a (cons b lst))))

(define seqR
  (lambda (a b lst)
    (cons b (cons a lst))))

(define insert-g
  (lambda (seq)
    (lambda (new-value needle haystack)
      (cond
        ((null? haystack)
         '())
        ((eq? (car haystack) needle)
         (seq new-value needle haystack))
        (else
          (cons
            (car haystack)
            ((insert-g seq) new-value needle (cdr haystack))))))))

(define insertL
  (insert-g
    (lambda (new-value needle haystack)
      (cons new-value (cons needle haystack)))))

(define insertR
  (insert-g
    (lambda (new-value needle haystack)
      (cons needle (cons new-value haystack)))))
