#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Positional List with Sentinel Search               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (positional-list-with-sentinel)
  (export new positional-list? find
          attach-first! attach-last! attach-middle!
          detach-first! detach-last! detach-middle!
          length empty? full? update! peek
          first last has-next? has-previous? next previous)
  (import
   (except (scheme base) length map for-each)
   ;(a-d positional-list vectorial))
   (a-d positional-list augmented-double-linked))
  (begin
    ; Sentinel Search only makes sense when having a O(1) access to the last element
    ; Otherwise inserting the sentinel itself takes O(n)!
    ; Hence, vectorial implementation or implementation augmented with a last-pointer

    (define (find plst key)
      (if (empty? plst)
          #f
          (let
              ((==? (equality plst)))
            (attach-last! plst key)
            (let*
                ((pos (let search-sentinel
                        ((curr (first plst)))
                        (if (==? (peek plst curr) key)
                            curr
                            (search-sentinel (next plst curr)))))
                 (res (if (has-next? plst pos)
                          pos
                          #f)))
              (detach-last! plst (last plst))
              res))))))
  