#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*               Lists With Current (Linked Version)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (list-with-current)
  (export new from-scheme-list list-with-current? 
          length empty? full? map
          set-current-to-first! set-current-to-last! current-has-next? current-has-previous?
          set-current-to-next! set-current-to-previous! has-current?
          add-after! add-before! delete! update! peek find!)
  (import (except (scheme base) length map))
  (begin
    (define-record-type list-with-current
      (make s h r c e)
      list-with-current?
      (s size size!)
      (h head head!)
      (r rear rear!)
      (c current current!)
      (e equality equality!))

    (define-record-type list-node
      (make-list-node v p n)
      list-node?
      (v list-node-val list-node-val!)
      (p list-node-prev list-node-prev!)
      (n list-node-next list-node-next!))
 
    (define (new ==?)
      (make 0 '() '() '() ==?))
 
    (define (from-scheme-list slst ==?)
      (define result (make 0 '() '() '() ==?))
      (if (not (null? slst))
          (let ((first (make-list-node (car slst) '() '())))
            (head! result first)
            (size! result 1)
            (rear!
             result
             (let loop
               ((scml (cdr slst))
                (curr first))
               (if (null? scml)
                   curr
                   (let ((node (make-list-node (car scml) curr '())))
                     (list-node-next! curr node)
                     (size! result (+ 1 (size result)))
                     (loop (cdr scml) node)))))))
      result)
 
    (define (length llwc)
      (size llwc))
 
    (define (full? llwc)
      #f)
 
    (define (empty? llwc)
      (null? (head llwc)))
 
    (define (map llwc func ==?)
      (if (empty? llwc)
          (new ==?)
          (let ((result (new ==?)))
            (set-current-to-first! llwc)
            (add-after! result (func (peek llwc)))
            (let loop
              ()
              (cond 
                ((current-has-next? llwc)
                 (set-current-to-next! llwc)
                 (add-after! result (func (peek llwc)))
                 (loop))))
            result)))
  
    (define (foldl llwc oper zero)
      (define (foldl-iter accu nlst)
        (if (null? nlst)
            accu
            (foldl-iter (oper accu (list-node-val nlst)) (list-node-next nlst))))
      (foldl-iter zero (head llwc)))
 
    (define (foldr llwc oper zero)
      (define (inner-foldr nlst)
        (if (null? nlst)
            zero
            (oper (list-node-val nlst) (inner-foldr (list-node-next nlst)))))
      (inner-foldr (head llwc)))
 
    (define (set-current-to-first! llwc)
      (if (null? (head llwc))
          (error "list empty (set-current-to-first!)" llwc))
      (current! llwc (head llwc))
      llwc)
 
    (define (set-current-to-last! llwc)
      (if (null? (head llwc))
          (error "list empty (set-current-to-last!)" llwc))
      (current! llwc (rear llwc))
      llwc)
 
    (define (current-has-next? llwc)
      (if (null? (current llwc))
          (error "no current (current-has-next?)" llwc)
          (not (null? (list-node-next (current llwc))))))
 
    (define (current-has-previous? llwc)
      (if (null? (current llwc))
          (error "no current (current-has-previous?)" llwc))
      (not (null? (list-node-prev (current llwc)))))
 
    (define (set-current-to-next! llwc)
      (if (not (current-has-next? llwc))
          (error "current has no next (set-current-to-next!)" llwc))
      (current!
       llwc 
       (list-node-next (current llwc)))
      llwc)
 
    (define (set-current-to-previous! llwc)
      (if (not (current-has-previous? llwc))
          (error "current has no previous (set-current-to-previous!)" llwc))
      (current!
       llwc 
       (list-node-prev (current llwc)))
      llwc)
 
    (define (has-current? llwc)
      (not (null? (current llwc))))
 
    (define (find! llwc key)
      (define ==? (equality llwc))
      (define (iter-to-key node)
        (cond 
          ((null? node) 
           (current! llwc '()))
          ((==? (list-node-val node) key) 
           (current! llwc node))
          (else
           (iter-to-key (list-node-next node)))))
      (if (null? (head llwc))
          (current! llwc '())
          (iter-to-key (head llwc)))
      llwc)
 
    (define (update! llwc val)
      (if (null? (current llwc))
          (error "no current (update!)" llwc))
      (list-node-val! (current llwc) val)
      llwc)
 
    (define (peek llwc)
      (if (null? (current llwc))
          (error "no current (peek)" llwc))
      (list-node-val (current llwc)))
 
    (define (add-before! llwc val)
      (define curr (current llwc))
      (define node '())
      (if (and (null? curr)
               (not (null? (head llwc))))
          (error "no current (add-before!" llwc))
      (set! node (make-list-node val '() curr))
      (if (or (null? curr) (eq? (head llwc) curr))
          (head! llwc node)
          (list-node-next! (list-node-prev curr) node))
      (if (not (null? curr))
          (list-node-prev! curr node))
      (if (null? curr)
          (rear! llwc node)
          (list-node-prev! node (list-node-prev curr)))
      (current! llwc node)
      (size! llwc (+ (size llwc) 1))
      llwc)
 
    (define (add-after! llwc val)
      (define curr (current llwc))
      (define node '())
      (if (and (null? curr)
               (not (null? (head llwc))))
          (error "no meaningful value (add-after!" llwc))
      (set! node (make-list-node val curr '()))
      (if (null? curr)
          (head! llwc node)
          (list-node-next! node (list-node-next curr)))
      (if (or (null? curr) (eq? (rear llwc) curr))
          (rear! llwc node)
          (list-node-prev! (list-node-next curr) node))
      (if (not (null? curr))                                 
          (list-node-next! curr node))
      (current! llwc node)
      (size! llwc (+ (size llwc) 1))
      llwc)
 
    (define (delete! llwc)
      (define curr (current llwc))
      (if (null? curr)
          (error "no current (delete!)" llwc))
      (if (null? (list-node-prev curr)) ; remove first
          (head! llwc (list-node-next curr))
          (list-node-next! (list-node-prev curr) 
                           (list-node-next curr)))
      (if (null? (list-node-next curr)) ; remove last
          (rear! llwc (list-node-prev curr))
          (list-node-prev! (list-node-next curr)
                           (list-node-prev curr)))
      (size! llwc (- (size llwc) 1))
      (current! llwc (list-node-next curr))
      llwc)))