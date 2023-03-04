#lang r7rs
;; Algoritmen en Datastructuren II: Union-Find
;; Oefening 3: Disjoint Sets met gelinkte lijsten

; Voordeel bij deze versie: je kan makkelijk achterhalen welke elementen zich in een set bevinden door de gelinkte lijst af te lopen.
; Bij de up-tree versies is dat niet rechtstreeks mogelijk aangezien pointers richting de root wijzen.
; In de naïeve versie van dit ADT is dit wel mogelijk, maar je zal altijd alle elementen in de vector moeten doorlopen.
; In de gelinkte versie kan het aantal te doorlopen nodes kleiner zijn (zolang er meer dan 1 set is).

(define-library (disjoint-sets)
  (export new disjoint-sets? find union! same-set?)
  (import (scheme base)
          (prefix (01-union-find Oplossingen uf-linked-list) lst:))

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
      (lst:list-node-val (lst:list-leader (lst:head (vector-ref (storage sets) nmbr)))))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      ; Het uitvoeren van lst:append! heeft een worst case performatie O(n),
      ; omdat alle nodes in de tweede set van leider veranderd dienen te worden.
      ; Bijgevolg heeft union! eveneens een worst case performantie O(n).
      (lst:append! (vector-ref (storage sets) set1)
                   (vector-ref (storage sets) set2))
      sets)))