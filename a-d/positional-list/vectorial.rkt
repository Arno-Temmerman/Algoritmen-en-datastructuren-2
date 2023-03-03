#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Vectorial Positional Lists                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (vector-positional-list)
  (export new positional-list? equality
          attach-first! attach-last! attach-middle!
          detach-first! detach-last! detach-middle!
          length empty? full? update! peek
          first last has-next? has-previous? next previous)
  (import (except (scheme base) length))
  (begin
    (define positional-list-size 10)

    (define-record-type positional-list
      (make v s e)
      positional-list?
      (v storage storage!)
      (s size size!)
      (e equality))

    (define (new ==?)
      (make (make-vector positional-list-size) 0 ==?))

    (define (storage-move-right vector i j)
      (do ((idx j (- idx 1)))
        ((< idx i))
        (vector-set! vector (+ idx 1) (vector-ref vector idx))))

    (define (storage-move-left vector i j)
      (do ((idx i (+ idx 1)))
        ((> idx j))
        (vector-set! vector (- idx 1) (vector-ref vector idx))))
 
    (define (attach-first! plst val)
      (attach-middle! plst val -1))

    (define (attach-middle! plst val pos)
      (define vect (storage plst))
      (define free (size plst))
      (storage-move-right vect (+ pos 1) (- free 1))
      (vector-set! vect (+ pos 1) val)
      (size! plst (+ free 1)))

    (define (attach-last! plst val)
      (define vect (storage plst))
      (define free (size plst))
      (vector-set! vect free val)
      (size! plst (+ free 1)))
 
    (define (detach-first! plst)
      (detach-middle! plst 0))
 
    (define (detach-last! plst pos)
      (define free (size plst))
      (size! plst (- free 1)))
 
    (define (detach-middle! plst pos)
      (define vect (storage plst))
      (define free (size plst))
      (storage-move-left vect (+ pos 1) (- free 1))
      (size! plst (- free 1)))
 
    (define (length plst)
      (size plst))
 
    (define (empty? plst)
      (= 0 (size plst)))
 
    (define (full? plst)
      (= (size plst)
         (vector-length (storage plst))))
 
    (define (first plst)
      (if (= 0 (size plst))
          (error "empty list (first)" plst)
          0))
 
    (define (last plst)
      (if (= 0 (size plst))
          (error "empty list (last)" plst)
          (- (size plst) 1)))
 
    (define (has-next? plst pos)
      (< (+ pos 1) (size plst)))
 
    (define (has-previous? plst pos)
      (< 0 pos))
 
    (define (next plst pos)
      (if (not (has-next? plst pos))
          (error "list has no next (next)" plst)
          (+ pos 1)))
 
    (define (previous plst pos)
      (if (not (has-previous? plst pos))
          (error "list has no previous (previous)" plst)
          (- pos 1)))
 
    (define (peek plst pos)
      (if (> pos (size plst))
          (error "illegal position (peek)" plst)
          (vector-ref (storage plst) pos)))
 
    (define (update! plst pos val)
      (if (> pos (size plst))
          (error "illegal position (update!)" plst)
          (vector-set! (storage plst) pos val)))))