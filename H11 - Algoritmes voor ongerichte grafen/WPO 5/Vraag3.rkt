#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d graph weighted config)
        (a-d graph examples undirected-weighted)
        (rename (a-d sorting internal comparative quicksort) (sort quicksort))
        (a-d scheme-tools)
        (prefix (a-d priority-queue modifiable-heap) pq:)
        (prefix (a-d disjoint-sets optimized) dset:))

;;Hulpstuk
(define (all-edges g)
  (define n-e (nr-of-edges g))
  (define edges (make-vector n-e))
  (define edge-count 0)
  (for-each-node
   g (lambda (from)
       (for-each-edge
        g from
        (lambda (weight to)
          (when (<= from to) ; don't take the reverse edges
            (vector-set! edges edge-count (cons from (cons weight to)))
            (set! edge-count (+ 1 edge-count)))))))
  edges)


;;Pas aan
(define (mst-prim-jarnik g)
   (define tree (make-vector (order g) '()))
   (define pq-ids (make-vector (order g) '()))
   (define mst-id 0)
   (define (track-ids id node weight) 
     (vector-set! pq-ids node id))
   (define (id-of node)
     (vector-ref pq-ids node))
   (define (pty< edge1 edge2)
     (< (cdr edge1) (cdr edge2)))
   (define pq (pq:new (order g) pty<))
   (define (prim-jarnik-iter closest-node&edge)
     (define closest-node (car closest-node&edge)) ;; deze knoop toevoegen aan MST
     (define closest-edge (cdr closest-node&edge)) ;; edge waarlangs we gekomen zijn
     (vector-set! tree closest-node (cons mst-id closest-edge))
     (for-each-edge
      g closest-node
      (lambda (weight to)
        (define edge-to-to (cons closest-node weight))
        (if (null? (id-of to))
            (pq:enqueue! pq to edge-to-to track-ids)
            (if (and (null? (vector-ref tree to))
                     (pty< edge-to-to (pq:priority-of pq (id-of to))))
                (pq:reschedule! pq (id-of to) edge-to-to track-ids)))))
     (unless (pq:empty? pq)
       (prim-jarnik-iter (pq:serve! pq track-ids))))
   (for-each-node g (lambda (node)
                      (when (null? (vector-ref tree node))
                        (set! mst-id (+ mst-id 1)) ; node onbereikbaar vanuit huidige tree -> volgende tree
                        (pq:enqueue! pq node (cons '() +inf.0) track-ids)
                        (prim-jarnik-iter (pq:serve! pq track-ids)))))
   tree)


;;Test Vraag 3
(display "MST van graf cormen571split, gevonden door algoritme van Prim-Jarník: ")
(newline)
(display (mst-prim-jarnik cormen571split))(newline)
(newline)

;;Output Vraag 3
;;MST van graf cormen571split, gevonden door algoritme van Prim-Jarník:
;;#({1 () . +inf.0} {1 0 . 4} {2 () . +inf.0}
;;  {2 2 . 7} {2 3 . 9} {2 2 . 4}
;;  {1 7 . 1} {1 0 . 8} {1 6 . 6})

;; b) Performance: still O(|E|*log(|V|))