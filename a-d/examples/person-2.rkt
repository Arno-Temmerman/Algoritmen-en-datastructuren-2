#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Person Implementation (vectors)                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (person)
  (export new person? name surname age age! salary salary!)
  (import (scheme base))
  (begin
    
    (define person-tag 'person)
 
    (define (new n sn a s)
      (vector person-tag n sn a s))
    
    (define tag-index 0)
    (define nam-index 1)
    (define sur-index 2)
    (define age-index 3)
    (define sal-index 4)
 
    (define (person? x)
      (and (vector? x)
           (eq? (vector-ref x tag-index) person-tag)))

    (define (name p)
      (vector-ref p nam-index))

    (define (surname p)
      (vector-ref p sur-index))

    (define (age p)
      (vector-ref p age-index))

    (define (age! p a)
      (vector-set! p age-index a)
      p)

    (define (salary p)
      (vector-ref p sal-index))
   
    (define (salary! p s)
      (vector-set! p sal-index s)
      p)))