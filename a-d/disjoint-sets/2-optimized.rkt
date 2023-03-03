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
  (export new disjoint-sets? find union! same-set?)
  (import (scheme base)
          (scheme write))

  (begin
    (define-record-type disjoint-sets
      (make t w)
      disjoint-sets?
      (t up-trees)
      (w tree-weights))

    ; new
    ; ( number ➙ disjoint-sets<ID> )
    (define (new size)
      (define trees (make-vector size 0))
      (define weights (make-vector size 1))
      (define sets (make trees weights))
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
      (define weights (tree-weights sets))
      (define trees (up-trees sets))
      (define (up-tree-rec elmt prev-weight)
        (if (not (eq? elmt (vector-ref trees elmt)))
            (let ((temp (vector-ref weights elmt)))
              (vector-set! weights elmt (- (vector-ref weights elmt) prev-weight))
              (vector-set! trees elmt (up-tree-rec (vector-ref trees elmt)
                                                   temp))))
        (vector-ref trees elmt))
      (up-tree-rec nmbr 0))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      (define weights (tree-weights sets))
      (define trees (up-trees sets))
      (define w1 (vector-ref weights set1))
      (define w2 (vector-ref weights set2))
      (cond ((> w1 w2)
             (vector-set! trees set2 set1)
             (vector-set! weights set1 (+ w1 w2)))
            (else 
             (vector-set! trees set1 set2)
             (vector-set! weights set2 (+ w1 w2))))
      sets)))

; Voorbeeld: unie van x en y
;; (union! s (find s x) (find s y))

(define tree (new 10))
(display tree)
(newline)
(display "UNION 1 2")
(newline)
(display (union! tree (find tree 1) (find tree 2)))
(newline)
(display "UNION 2 3")
(display (union! tree (find tree 2) (find tree 3)))