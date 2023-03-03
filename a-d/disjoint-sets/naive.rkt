#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Disjoint Sets (Naive Version)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2012 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (disjoint-sets)
  (export new disjoint-sets? find union! same-set?)
  (import (scheme base))

  (begin
 
    (define-record-type disjoint-sets
      (make i)
      disjoint-sets?
      (i identities))

    ; new
    ; ( number ➙ disjoint-sets<ID> )
    (define (new size)
      (define idts (make-vector size 0))
      (define sets (make idts))
      (let fill-singletons 
        ((i 0))
        (vector-set! idts i i)
        (if (< (+ 1 i) size)
            (fill-singletons (+ i 1))))
      sets)
    
    ; same-set?
    ; ( ID ID ➙ boolean )
    (define same-set? =) 

    ; find
    ; ( disjoint-sets<ID> number ➙ ID )
    (define (find sets nmbr)
      (define idts (identities sets))
      (vector-ref idts nmbr))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      (define idts (identities sets))
      (define size (vector-length idts))
      (do ((i 0 (+ i 1)))
        ((= i size) sets)
        (if (= (vector-ref idts i) set1)
            (vector-set! idts i set2))))))