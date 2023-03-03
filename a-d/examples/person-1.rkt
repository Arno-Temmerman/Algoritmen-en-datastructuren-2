#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Person Implementation (liss)                   *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (person)
  (export new person? name surname age age! salary salary!)
  (import (scheme base)
          (scheme cxr))
  (begin

    (define (new name snam age sal)
      (list 'person name snam age sal))

    (define (person? sval)
      (and (pair? sval) (eq? (car sval) 'person)))
    
    (define (name prsn)
      (cadr prsn))

    (define (surname prsn)
      (caddr prsn))

    (define (age prsn)
      (cadddr prsn))

    (define (age! prsn)
      (set-car! (cdddr prsn)))

    (define (salary prsn)
      (set-car! (cddddr prsn)))

    (define (salary! prsn slry)
      (set-car! (cddddr prsn) slry))))
