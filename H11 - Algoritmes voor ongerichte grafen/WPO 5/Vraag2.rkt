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
(define (mst-kruskal g)
  (define edges (all-edges g))
  (define n-e (vector-length edges))
  (define forest '())
  (define node-sets (dset:new (order g)))
  (quicksort edges (lambda (edge1 edge2) (< (cadr edge1) (cadr edge2))))
  (do ((edge-idx 0 (+ edge-idx 1)))
      ((= edge-idx n-e) forest)
    (let* ((edge (vector-ref edges edge-idx))
	   (from (car edge))
	   (weight (cadr edge))
	   (to (cddr edge))
	   (from-set (dset:find node-sets from))
	   (to-set (dset:find node-sets to)))
      (when (not (dset:same-set? from-set to-set)) 
        (set! forest (cons (list from weight to) forest))
        (dset:union! node-sets from-set to-set)))))


;;Test Vraag 2
(display "MST van graf cormen571split, gevonden door algoritme van Kruskal: ")(newline)
(display (mst-kruskal cormen571split))(newline)
(newline)

;;Output Vraag 2
;;MST van graf cormen571split, gevonden door algoritme van Kruskal:
;;{{ID FROM WEIGHT TO} ...}
;;{{5 3 9 4} {7 0 8 7} {5 2 7 3} {7 6 6 8} {7 0 4 1} {5 2 4 5} {7 6 1 7}}


