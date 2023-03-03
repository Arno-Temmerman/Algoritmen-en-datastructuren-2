#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Linked Positional Lists                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2011  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (linked-positional-list)
  (export new positional-list? equality
          attach-first! attach-last! attach-middle!
          detach-first! detach-last! detach-middle!
          length empty? full? update! peek
          first last has-next? has-previous? next previous)
  (import (except (scheme base) length))
  (begin
    (define make-list-node cons)
    (define list-node-val car)
    (define list-node-val! set-car!)
    (define list-node-next cdr)
    (define list-node-next! set-cdr!)
 
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
            (chasing-pointers 
             next 
             (list-node-next next)))))
 
    (define (attach-first! plst val)
      (define frst (head plst))
      (define node (make-list-node val frst))
      (head! plst node))
 
    (define (attach-middle! plst val pos)
      (define next (list-node-next pos))
      (define node (make-list-node val next))
      (list-node-next! pos node))
 
    (define (attach-last! plst val)
      (define last (iter-from-head-until plst null?))
      (define node (make-list-node val '()))
      (define frst (head plst))
      (if (null? frst)
          (head! plst node) ; last is also first
          (list-node-next! last node)))
 
    (define (detach-first! plst)
      (define frst (head plst))
      (define scnd (list-node-next frst))
      (head! plst scnd))
 
    (define (detach-middle! plst pos)
      (define next (list-node-next pos))
      (define prev (iter-from-head-until 
                    plst 
                    (lambda (node) (eq? pos node))))
      (list-node-next! prev next))
 
    (define (detach-last! plst pos)
      (define frst (head plst))
      (define scnd (list-node-next frst))
      (if (null? scnd) ; last is also first
          (head! plst '())
          (list-node-next! (iter-from-head-until 
                            plst 
                            (lambda (last) (not (has-next? plst last)))) 
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
          (iter-from-head-until plst (lambda (node) (eq? pos node)))))
 
    (define (update! plst pos val)
      (list-node-val! pos val)
      plst)
 
    (define (peek plst pos)
      (list-node-val pos))))