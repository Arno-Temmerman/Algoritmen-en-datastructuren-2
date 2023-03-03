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
  (import (scheme base))

  (begin
    (define-record-type disjoint-sets
      (make t)
      disjoint-sets?
      (t up-trees))

    ; new
    ; ( number ➙ disjoint-sets<ID> )
    (define (new size)
      (define trees (make-vector size -1)) ; vul elke cel met rang -1
      (define sets (make trees))
      sets)

    ; same-set?
    ; ( ID ID ➙ boolean )
    (define same-set? =)

    ; find
    ; ( disjoint-sets<ID> number ➙ ID )
    (define (find sets nmbr)
      (define trees (up-trees sets))
      (define (up-tree-rec elmt)
        ; Als waarde positief is, zijn we nog niet aan de root
        (if (not (> 0 (vector-ref trees elmt)))
            (begin
              ; Doe padcompressie
              (vector-set! trees elmt (up-tree-rec (vector-ref trees elmt)))
              ; Geef de (positieve) ID van de root terug voor de volgende in de recursie
              (vector-ref trees elmt))
            ; Anders zijn we wel aan de root en geven we de ID van de root terug
            elmt))
      (up-tree-rec nmbr))

    ; union!
    ; ( disjoint-sets<ID> ID ID ➙ disjoint-sets<ID> )
    (define (union! sets set1 set2)
      (define trees (up-trees sets))
      (cond ((< (vector-ref trees set1) 
                (vector-ref trees set2))
             (vector-set! trees set2 set1))
            ; Enkel wanneer twee sets van zelfde rank gejoind worden, moet de rank geüpdatet worden
            ((= (vector-ref trees set1) 
                (vector-ref trees set2))
             (vector-set! trees set1 set2)
             (vector-set! trees set2 (- (vector-ref trees set2) 1)))
            (else
             (vector-set! trees set1 set2)))
      sets)))

; Voorbeeld: unie van x en y
;; (union! s (find s x) (find s y))