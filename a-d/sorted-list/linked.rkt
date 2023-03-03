#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Sorted Lists (Linked Implementation)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (linked-sorted-list)
  (export new from-scheme-list sorted-list? empty? full? length
          find! delete! add! peek
          set-current-to-first! set-current-to-next! has-current? current-has-next?)
  (import (except (scheme base) length))
  (begin
    (define-record-type sorted-list
      (make-sorted-list s h c l e)
      sorted-list?
      (s size size!)
      (h head head!)
      (c current current!)
      (l lesser)
      (e equality))

    (define-record-type list-node
      (make-list-node v n)
      list-node?
      (v list-node-val list-node-val!)
      (n list-node-next list-node-next!))
 
    (define (new <<? ==?)
      (make-sorted-list 0 '() '() <<? ==?))
 
    (define (from-scheme-list slst <<? ==?)
      (let list-rec
        ((lst slst))
        (if (null? lst)
            (new <<? ==?)
            (add! (list-rec (cdr lst)) (car lst)))))
 
    (define (length slst)
      (size slst))
 
    (define (empty? slst)
      (= (size slst) 0))
 
    (define (full? slst)
      #f)
 
    (define (find! slst key)
      (define ==? (equality slst))
      (define <<? (lesser slst))
      (let list-iter
        ((node (head slst)))
        (cond
          ((null? node) 
           (current! slst '()))
          ((==? key (list-node-val node))
           (current! slst node))
          ((<<? (list-node-val node) key)
           (list-iter (list-node-next node)))
          (else 
           (current! slst '())))))
 
    (define (delete! slst)
      (define first (head slst))
      (define curr (current slst))
      (define (iter-to-previous prev next! next)
        (cond 
          ((eq? next curr)
           (next! prev (list-node-next next)))
          (else 
           (iter-to-previous 
            next list-node-next! (list-node-next next)))))
      (if (not (has-current? slst))
          (error "no current (delete!)" slst))
      (iter-to-previous 
       '() 
       (lambda (ignore node) (head! slst node)) 
       (head slst))
      (size! slst (- (size slst) 1))
      (current! slst '())
      slst)
 
    (define (peek slst)
      (if (not (has-current? slst))
          (error "no current (peek)" slst)
          (list-node-val (current slst))))
 
    (define (add! slst val)
      (define ==? (equality slst))
      (define <<? (lesser slst))
      (define (insert-node prev next! next)
        (let ((node (make-list-node val next)))
          (current! slst node)
          (next! prev node)))
      (define (iter-to-position prev next! next)
        (cond
          ((or (null? next)
               (<<? val (list-node-val next)))
           (insert-node prev next! next))
          (else
           (iter-to-position next list-node-next! (list-node-next next)))))
      (iter-to-position
       '()
       (lambda (ignore node) (head! slst node))
       (head slst))
      (size! slst (+ (length slst) 1))
      slst)
 
    (define (set-current-to-first! slst)
      (current! slst (head slst)))
 
    (define (set-current-to-next! slst)
      (if (not (has-current? slst))
          (error "current has no meaningful value (set-current-to-next!" slst)
          (current! slst (list-node-next (current slst)))))
 
    (define (has-current? slst)
      (not (null? (current slst))))
 
    (define (current-has-next? slst)
      (if (not (has-current? slst))
          (error "no Current (current-has-next?" slst)
          (not (null? (list-node-next (current slst))))))))
  