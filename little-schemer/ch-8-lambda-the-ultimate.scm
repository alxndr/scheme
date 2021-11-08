(load "ch-6-shadows.scm")

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

(define eq?-curry ; "eq?-c"
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

(define subst
  (insert-g
    (lambda (new-value needle haystack)
      (cons new-value (cdr haystack)))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) plus)
      ((eq? x '*) times)
      (else exponent))))

(define value
  (lambda (nexp)
    (cond
      ((number? nexp)
       nexp)
      (else
        ((atom-to-function (car nexp))
         (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test)
    (lambda (atm lst)
      (cond
        ((null? lst)
         '())
        ((test (car lst) atm)
         ((multirember-f test) atm (cdr lst)))
        (else
          (cons (car lst)
                ((multirember-f test) atm (cdr lst))))))))

(define multirember-eq (multirember-f eq?))

(define multiremberT
  (lambda (test-fun listofatoms)
    (cond
      ((null? listofatoms)
       '())
      ((test-fun (car listofatoms))
       (multiremberT test-fun (cdr listofatoms)))
      (else
        (cons (car listofatoms)
              (multiremberT test-fun (cdr listofatoms)))))))
  ; > (multiremberT (eq?-curry 'salad) '(shrimp salad tuna salad and tuna melt))
  ; '(shrimp tuna and tuna melt)

(define multirember&co
  ; "looks at every atom in [list-of-atoms] to see whether it is `eq?` to [atm].
  ; "Those … that are not are collected in one list… the others … are collected in a second list.
  ; "Finally it determines the value of `(collector new-list-of-atoms seen)`"
  ; illustrating 10th Commandment: Build fxns to collect more than one value at a time (iteration)
  (lambda (atm list-of-atoms collector)
    (cond

      ((null? list-of-atoms)
       (collector '()) '())

      ((eq? (car list-of-atoms) atm)                                    ; when `atm` is in `list-of-atoms`…
       (multirember&co atm                                              ; recurse…
                       (cdr list-of-atoms)                              ; through rest of `list-of-atoms`.
                       (lambda (new-list-of-atoms seen)                 ; final function will include…
                         (collector new-list-of-atoms
                                    (cons (car list-of-atoms) seen))))) ; the `atm` in the `seen` list

      (else                                                              ; when `atm` not in `list-of-atoms`…
        (multirember&co atm                                              ; recurse…
                        (cdr list-of-atoms)                              ; through rest of `list-of-atoms`.
                        (lambda (new-list-of-atoms seen)                 ; final function will include…
                          (collector
                            (cons (car list-of-atoms) new-list-of-atoms) ; `atm` in the `new…` list
                            seen)))))))

(define multiinsertLR
  (lambda (new-value needleL needleR haystack)
    (cond
      ((null? haystack)
       '())
      ((eq? needleL (car haystack))
       (cons new-value (cons needleL (multiinsertLR new-value needleL needleR (cdr haystack)))))
      ((eq? needleR (car haystack))
       (cons needleR (cons new-value (multiinsertLR new-value needleL needleR (cdr haystack)))))
      (else
        (cons (car haystack)
              (multiinsertLR new-value needleL needleR (cdr haystack)))))))

(define multiinsertLR&co ; collector version of above function
  (lambda (new-value needleL needleR haystack collector)
    ; collector function signature: (new-haystack num-left-insertions num-right-insertions)
    (cond
      ((null? haystack)
       (collector '() 0 0))

      ((eq? needleL (car haystack))
       (multiinsertLR&co new-value
                         needleL
                         needleR
                         (cdr haystack)
                         (lambda (new-haystack num-left-insertions num-right-insertions)
                           (collector (cons new-value (cons needleL (cdr haystack))) ; or new-haystack ??
                                      (plus 1 num-left-insertions)
                                      num-right-insertions))))
      ((eq? needleR (car haystack))
       (multiinsertLR&co new-value
                         needleL
                         needleR
                         (cdr haystack)
                         (lambda (new-haystack num-left-insertions num-right-insertions)
                           (collector (cons needleR (cons new-value (cdr haystack))) ; or new-haystack ??
                                      num-left-insertions
                                      (plus 1 num-right-insertions)))))
      (else
        (multiinsertLR&co new-value
                          needleL
                          needleR
                          (cdr haystack)
                          (lambda (new-haystack num-left-insertions num-right-insertions)
                            (collector haystack
                                       num-left-insertions
                                       num-right-insertions)))))))

(define even?
  (lambda (n)
    (eq? (times (div n 2) 2)
         n)))

; "*-functions work on lists that are either empty;
; "an atom `cons`ed onto a list;
; "or a list `cons`ed onto a list"

(define evens-only*
  (lambda (list-of-lists)
    (cond
      ((null? list-of-lists)
       '())
      ((atom? (car list-of-lists))
       (cond
         ((even? (car list-of-lists))
          (cons (car list-of-lists)
                (evens-only* (cdr list-of-lists))))
         (else
           (evens-only* (cdr list-of-lists)))))
      (else
        (cons (evens-only* (car list-of-lists))
              (evens-only* (cdr list-of-lists)))))))

(define evens-only*&co
  (lambda (list-of-lists collector)
    ; collector fn sig: (list-of-lists-evens evens-multiplied odds-summed)
    (cond
      ((null? list-of-lists)
       (collector '() 1 0))
      ((atom? (car list-of-lists))
       (cond
         ((even? (car list-of-lists))
          (evens-only*&co (cdr list-of-lists)
                          (lambda (list-of-lists-evens evens-multiplied odds-summed)
                            (collector (cons (car list-of-lists) list-of-lists-evens)
                                       (times (car list-of-lists) evens-multiplied)
                                       odds-summed))))
         (else
           (evens-only*&co (cdr list-of-lists)
                           (lambda (list-of-lists-evens evens-multiplied odds-summed)
                             (collector list-of-lists-evens
                                        evens-multiplied
                                        (plus (car list-of-lists) odds-summed)))))))
      (else
        (evens-only*&co (car list-of-lists)
                        (lambda (list-of-lists-evens evens-multiplied odds-summed)
                          (evens-only*&co (cdr list-of-lists)
                                          (lambda (nestedL nestedP nestedS)
                                            (collector (cons list-of-lists-evens nestedL)
                                                       (times evens-multiplied nestedP)
                                                       (plus odds-summed nestedS))))))))))
