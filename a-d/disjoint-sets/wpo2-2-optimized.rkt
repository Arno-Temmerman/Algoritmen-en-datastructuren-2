#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Disjoint Sets (Path Compression)                *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2012 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (disjoint-sets)
  (export new disjoint-sets? find union! same-set?
          number-of-sets)
  (import (scheme base))

  (begin
    (define-record-type disjoint-sets
      (make t r ns)
      disjoint-sets?
      (t up-trees)
      (r tree-ranks)
      (ns number-of-sets number-of-sets!))

    ; new
    ; ( number ➙ disjoint-sets<ID> )
    (define (new size)
      (define trees (make-vector size 0))
      (define ranks (make-vector size 0))
      (define sets (make trees ranks size))
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
      (define (up-tree-rec elmt)
        (if (not (eq? elmt (vector-ref trees elmt)))
            (vector-set! trees elmt (up-tree-rec (vector-ref trees elmt))))
        (vector-ref trees elmt))
      (up-tree-rec nmbr))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      (define ranks (tree-ranks sets))
      (define trees (up-trees sets))
      (when (not (same-set? set1 set2))
        (number-of-sets! sets (- (number-of-sets sets) 1))
        (cond ((> (vector-ref ranks set1) 
                  (vector-ref ranks set2))
               (vector-set! trees set2 set1))
              ((= (vector-ref ranks set1) 
                  (vector-ref ranks set2))
               (vector-set! trees set1 set2)
               (vector-set! ranks set2 (+ 1 (vector-ref ranks set2))))
              (else
               (vector-set! trees set1 set2))))
      sets)))

; Voorbeeld: unie van x en y
;; (union! s (find s x) (find s y))