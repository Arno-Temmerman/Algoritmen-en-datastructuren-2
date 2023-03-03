#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Disjoint Sets (Up-Tree Implementation)             *-*-
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
      (make t)
      disjoint-sets?
      (t up-trees))

    ; new
    ; ( number ➙ disjoint-sets<ID> )
    (define (new size)
      (define trees (make-vector size 0))
      (define sets (make trees))
      (let fill-singletons 
        ((i 0))
        (vector-set! trees i i)
        (if (< (+ 1 i) size)
            (fill-singletons (+ i 1))))
      sets)

    ; same-set?
    ; ( ID ID ➙ boolean )
    (define same-set? =)

    ; find
    ; ( disjoint-sets<ID> number ➙ ID )
    (define (find sets nmbr)
      (define trees (up-trees sets))
      (let up-tree-loop
        ((elmt nmbr))
        (if (= elmt (vector-ref trees elmt))
            elmt
            (up-tree-loop (vector-ref trees elmt)))))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      (define trees (up-trees sets))
      (vector-set! trees set1 set2)
      sets)))