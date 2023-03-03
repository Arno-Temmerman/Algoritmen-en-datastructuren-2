#lang r7rs
;; Algoritmen en Datastructuren II: Union-Find
;; Oefening 3: Disjoint Sets met gelinkte lijsten

(define-library (disjoint-sets)
  (export new disjoint-sets? find union! same-set?)
  (import (scheme base)
          (prefix (01-union-find Opgave uf-linked-list) lst:))

  (begin
    (define-record-type disjoint-sets
      (make s)
      disjoint-sets?
      (s storage))

    ; new
    ; ( number ➙ disjoint-sets<ID> )
    ; Maak een opslag van singletons.
    (define (new size)
      (define set-vec (make-vector size))
      (define disj (make set-vec))
      (let fill-singletons 
        ((i 0))
        (vector-set! set-vec i (lst:add! (lst:new same-set?) i))
        (if (< (+ i 1) size)
            (fill-singletons (+ i 1))))
      disj)

    ; same-set?
    ; ( ID ID ➙ boolean )
    (define same-set? =)

    ; find
    ; ( disjoint-sets<ID> number ➙ ID )
    (define (find sets nmbr)
      (error 'todo))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      (error 'todo))
    ))