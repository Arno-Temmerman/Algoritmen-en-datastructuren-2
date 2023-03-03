#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Double Linked Positional Lists                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2011  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (double-positional-list)
  (export new positional-list? equality
          attach-first! attach-last! attach-middle!
          detach-first! detach-last! detach-middle!
          length empty? full? update! peek
          first last has-next? has-previous? next previous)
  (import (except (scheme base) length))
  (begin
   
    (define-record-type list-node
      (make-list-node v p n)
      list-node?
      (v list-node-val list-node-val!)
      (p list-node-prev list-node-prev!)
      (n list-node-next list-node-next!))
 
    (define-record-type positional-list 
      (make h e)
      positional-list?
      (h head head!)
      (e equality))
 
    (define (new ==?)
      (make '() ==?))
 
    (define (iter-from-head-until plst stop?)
      (define frst (head plst))
      (let chasing-pointers
        ((prev '())
         (next frst))
        (if (stop? next)
            prev
            (chasing-pointers next (list-node-next next)))))
 
    (define (attach-first! plst val)
      (define frst (head plst))
      (define node (make-list-node val '() frst))
      (head! plst node)
      (if (not (null? frst))
          (list-node-prev! frst node)))
 
    (define (attach-middle! plst val pos)
      (define next (list-node-next pos))
      (define node (make-list-node val pos next))
      (list-node-next! pos node)
      (if (not (null? next))
          (list-node-prev! next node)))
 
    (define (attach-last! plst val)
      (define last (iter-from-head-until plst null?))
      (define node (make-list-node val last '()))
      (define frst (head plst))
      (if (null? frst)
          (head! plst node) ; last is also first
          (list-node-next! last node)))
 
    (define (detach-first! plst)
      (define frst (head plst))
      (define scnd (list-node-next frst))
      (head! plst scnd)
      (if (not (null? scnd))
          (list-node-prev! scnd '())))
 
    (define (detach-middle! plst pos)
      (define next (list-node-next pos))
      (define prev (list-node-prev pos))
      (list-node-next! prev next)
      (list-node-prev! next prev))
 
    (define (detach-last! plst pos)
      (define frst (head plst))
      (define scnd (list-node-next frst))
      (if (null? scnd) ; last is also first
          (head! plst '())
          (list-node-next! (list-node-prev pos) 
                           '())))
 
    (define (length plst)
      (let length-iter
        ((curr (head plst))
         (size 0))
        (if (null? curr)
            size
            (length-iter (list-node-next curr) (+ size 1)))))
 
    (define (full? plst)
      #f)
 
    (define (empty? plst)
      (null? (head plst)))
 
    (define (first plst)
      (if (null? (head plst))
          (error "list empty (first)" plst)
          (head plst)))
 
    (define (last plst)
      (if (null? (head plst))
          (error "list empty (last)" plst)
          (iter-from-head-until plst null?)))
 
    (define (has-next? plst pos)
      (not (null? (list-node-next pos))))
 
    (define (has-previous? plst pos)
      (not (eq? pos (head plst))))
 
    (define (next plst pos)
      (if (not (has-next? plst pos))
          (error "list has no next (next)" plst)
          (list-node-next pos)))
 
    (define (previous plst pos)
      (if (not (has-previous? plst pos))
          (error "list has no previous (previous)" plst)
          (list-node-prev pos)))
 
    (define (update! plst pos val)
      (list-node-val! pos val)
      plst)
 
    (define (peek plst pos)
      (list-node-val pos))))