#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                             Rings                               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (ring)
  (export new from-scheme-list
          length ring?
          add-before! add-after! shift-forward! shift-backward!
          peek delete! update!)
  (import (except (scheme base) length))

  (begin 
    (define-record-type ring
      (make-ring c)
      ring?
      (c current current!))
   
    (define (new)
      (make-ring '()))

    (define make-ring-node cons)
    (define ring-node-val car)
    (define ring-node-val! set-car!)
    (define ring-node-next cdr)
    (define ring-node-next! set-cdr!)
 
    (define (from-scheme-list slst)
      (let loop
        ((scml slst)
         (ring (new)))
        (if (null? scml)
            ring
            (loop (cdr scml) (add-after! ring (car scml))))))
 
    (define (add-after! ring val)
      (define curr (current ring))
      (define node (make-ring-node val '()))
      (ring-node-next! node 
                       (if (null? curr)
                           node
                           (ring-node-next curr)))
      (if (not (null? curr))
          (ring-node-next! curr node))
      (current! ring node) 
      ring)
 
    (define (iter-to-previous node)
      (let chasing-pointers
        ((prev node)
         (next (ring-node-next node)))
        (if (eq? node next)
            prev
            (chasing-pointers next (ring-node-next next)))))
 
    (define (add-before! ring val)
      (define curr (current ring))
      (define node (make-ring-node val curr))
      (ring-node-next!
       (if (null? curr)
           node
           (iter-to-previous curr))
       node)
      (current! ring node)
      ring)
 
    (define (shift-forward! ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (shift-forward!)" ring))
      (current! ring (ring-node-next curr))
      ring)
 
    (define (shift-backward! ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (shift-backward!)" ring)
          (current! ring (iter-to-previous curr)))
      ring)
 
    (define (delete! ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (delete!)" ring))
      (ring-node-next! 
       (iter-to-previous curr)
       (ring-node-next curr))
      (if (eq? curr (ring-node-next curr))
          (current! ring '())
          (current! ring (ring-node-next curr)))
      ring)
 
    (define (update! ring val)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (update!)"ring)
          (ring-node-val! curr val)))
 
    (define (peek ring)
      (define curr (current ring))
      (if (null? curr)
          (error "empty ring (peek)" ring)
          (ring-node-val curr)))
 
    (define (length ring)
      (define curr (current ring))
      (if (null? curr)
          0
          (let loop
            ((pointer (ring-node-next curr))
             (acc 1))
            (if (eq? pointer curr)
                acc
                (loop (ring-node-next pointer) (+ acc 1))))))))