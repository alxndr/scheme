(define rember
  (lambda (needle haystack)
    (cond
      ((null? haystack) '())
      ((eq? (car haystack) needle) (cdr haystack))
    (else
      (cons (car haystack)
            (rember needle (cdr haystack)))))))

(define firsts
  (lambda (list-of-lists)
    (cond
      ((null? list-of-lists) '())
      ; ((null? (car list-of-lists)) #f)
      ; ((null? (cdr list-of-lists)) #f)
    (else
      (cons (car (car list-of-lists))
            (firsts (cdr list-of-lists)))))))

(define insertR
  (lambda (new-value needle haystack)
    (cond
      ((null? haystack) '())
      ((eq? (car haystack) needle) (cons needle (cons new-value (cdr haystack))))
      (else (cons (car haystack)
                  (insertR new-value needle (cdr haystack))))
    )
  )
)

(define insertL (lambda (new-value needle haystack)
                  (cond
                    ((null? haystack)
                     '())
                    ((eq? (car haystack) needle)
                     (cons new-value haystack))
                    (else
                      (cons (car haystack) (insertL new-value needle (cdr haystack)))
                      ))))

(define subst (lambda (new-value needle haystack)
                (cond
                  ((null? haystack)
                   '())
                  ((eq? needle (car haystack))
                   (cons new-value (cdr haystack)))
                  (else
                    (cons (car haystack) (subst new-value needle (cdr haystack)))))))

(define subst2 (lambda (new-value needle1 needle2 haystack)
                       (cond
                         ((null? haystack)
                          '())
                         ((or
                            (eq? needle1 (car haystack))
                            (eq? needle2 (car haystack)))
                          (cons new-value (cdr haystack)))
                         (else
                           (cons (car haystack) (subst2 new-value needle2 needle2 (cdr haystack)))))))

(define multirember (lambda (needle haystack)
                      (cond
                        ((null? haystack) '())
                        ((eq? (car haystack) needle)
                         (multirember needle (cdr haystack)))
                        (else
                          (cons (car haystack) (multirember needle (cdr haystack)))))))

(define multiinsertR (lambda (new-value needle haystack)
                       (cond
                         ((null? haystack) '())
                         ((eq? needle (car haystack))
                          (cons needle (cons new-value (multiinsertR new-value needle (cdr haystack)))))
                         (else
                           (cons (car haystack) (multiinsertR new-value needle (cdr haystack)))))))

(define multiinsertL (lambda (new-value needle haystack)
                       (cond
                         ((null? haystack) '())
                         ((eq? needle (car haystack))
                          (cons new-value (cons needle (multiinsertL new-value needle (cdr haystack)))))
                         (else
                           (cons (car haystack) (multiinsertL new-value needle (cdr haystack)))))))

(define multisubst (lambda (new-value needle haystack)
                     (cond
                       ((null? haystack) '())
                       ((eq? needle (car haystack))
                        (cons new-value (multisubst new-value needle (cdr haystack))))
                       (else
                         (cons (car haystack) (multisubst new-value needle (cdr haystack)))))))
