#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*          Labeled Graphs (Adjacency List Representation)         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2009  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

;; b) Net zoals bij ongelabelde grafen is de adjacency list representatie
;;    voordeliger wanneer we te maken hebben met ijle grafen.
;;    Het nadeel t.o.v. de gelabelde graf implementatie met een adjacency matrix is dat het
;;    opvragen van de label van een edge in O(|V|) gebeurt, terwijl dat met de matrix in O(1) kan.

(define-library (labeled-graph)
  (export new labeled-graph? order directed? nr-of-edges 
          for-each-node for-each-edge
          add-edge! delete-edge!
          adjacent?
          label label! edge-label)
  (import (scheme base))
  (begin
 
    (define-record-type labeled-graph
      (make d n s nl)
      labeled-graph?
      (d directed?)
      (n nr-of-edges nr-of-edges!)
      (s storage)
      (nl node-labels node-labels!))

    (define make-graph-edge cons)
    (define graph-edge-label car)
    (define graph-edge-to cdr)
 
    (define (new directed nr-of-nodes)
      (make directed 0 (make-vector nr-of-nodes '()) (make-vector nr-of-nodes 'no-label)))

    (define (order graph)
      (vector-length (storage graph)))
 
    (define (for-each-node graph proc)
      (define lists (storage graph))
      (let iter-nodes
        ((node 0))
        (proc node (label graph node))
        (if (< (+ node 1) (vector-length lists))
            (iter-nodes (+ node 1))))
      graph)
 
    (define (for-each-edge graph from proc)
      (define row (vector-ref (storage graph) from))
      (let iter-edges
        ((edges row))
        (if (not (null? edges))
            (let ((edge (car edges)))
              (proc (graph-edge-to edge) (graph-edge-label edge))
              (iter-edges (cdr edges)))))
      graph)
 
    (define (add-edge! graph from to label)
      (define lists (storage graph))
      (define edge (make-graph-edge label to))
      (define (insert-sorted edge prev next! next)
        (cond 
          ((or (null? next)
               (> (graph-edge-to edge) (graph-edge-to (car next))))
           (next! prev (cons edge next))
           #t)
          ((= (graph-edge-to edge) (graph-edge-to (car next)))
           #f)
          (else
           (insert-sorted edge next set-cdr! (cdr next)))))
      (define (head-setter head) 
        (lambda (ignore next)
          (vector-set! lists head next)))
      (if (insert-sorted edge '() (head-setter from) (vector-ref lists from))
          (nr-of-edges! graph (+ 1 (nr-of-edges graph))))
      (if (not (directed? graph))
          (let ((reverse (make-graph-edge label from)))
            (insert-sorted reverse '() (head-setter to) (vector-ref lists to))))
      graph)
 
    (define (delete-edge! graph from to)
      (define lists (storage graph))
      (define (delete-sorted to prev next! next)
        (cond 
          ((or (null? next)
               (> to (graph-edge-to (car next))))
           #f)
          ((= to (graph-edge-to (car next)))
           (next! prev (cdr next))
           #t)
          (else
           (delete-sorted to next set-cdr! (cdr next)))))
      (define (head-setter head) 
        (lambda (ignore next)
          (vector-set! lists head next)))
      (if (delete-sorted to '() (head-setter from) (vector-ref lists from))
          (nr-of-edges! graph (- (nr-of-edges graph) 1)))
      (if (not (directed? graph))
          (delete-sorted from '() (head-setter to) (vector-ref lists to)))
      graph)
 
    (define (adjacent? graph from to)
      (not (= (label graph from to) +inf.0)))
    
    (define (label! graph node label)
      (vector-set! (node-labels graph) node label))
    
    (define (label graph node)
      (vector-ref (node-labels graph) node))

    (define (edge-label graph from to)
      (define rows (storage graph))
      (if (eq? (vector-ref (cdr (vector-ref rows from)) to) 'no-label)
          #f
          (vector-ref (cdr (vector-ref rows from)) to)))
    )
  )