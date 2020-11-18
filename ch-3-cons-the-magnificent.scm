(define rember
  (lambda (needle haystack)
    (cond
      ((null? haystack) '())
      ((eq? (car haystack) needle) (cdr haystack))
    (else
      (cons (car haystack) (rember needle (cdr haystack)))
    ))
  )
)

(define firsts
  (lambda (list-of-lists)
    (cond
      ((null? list-of-lists) '())
      ; ((null? (car list-of-lists)) #f)
      ; ((null? (cdr list-of-lists)) #f)
    (else
      (cons (car (car list-of-lists)) (firsts (cdr list-of-lists)))
    ))
  )
)

(define insertR
  (lambda (new-value needle haystack)
    (cond
      ((null? haystack) '())
      ((eq? (car haystack) needle) (cons needle (cons new-value (cdr haystack))))
    (else
      (cons (car haystack) (insertR replacement needle (cdr haystack)))
    ))
  )
)

(define insertL ; not working...... dunno why though
  (lambda (new-value needle haystack)
    (cond
      ((null? haystack) '())
      ((eq? (car haystack) needle) (cons new-value (cons needle (cdr haystack))))
    (else
      (cons (car haystack) (insertL replacement needle (cdr haystack)))
    ))
  )
)
