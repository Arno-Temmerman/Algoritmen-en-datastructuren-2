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
(define (mst-boruvka g)
   (define n-e (nr-of-edges g))
   (define edges (all-edges g))
   (define infty-edge (cons 0 (cons +inf.0 0)))
   (define (edge< edge1 edge2)
     (< (cadr edge1) (cadr edge2)))
   (define selected-edges (make-vector (order g) infty-edge))
   (define node-sets (dset:new (order g)))
   (define (select-edges remaining-edges)
     (let find-minimals
       ((edge-idx 0)
        (first-free 0))
       (if (< edge-idx remaining-edges)
           (let* ((edge (vector-ref edges edge-idx))
                  (from (car edge))
                  (to   (cddr edge))
                  (from-set (dset:find node-sets from))
                  (to-set   (dset:find node-sets to)))
             (cond ((dset:same-set? from-set to-set)
                    ; edge already in MST-forest
                    ; skip it
                    (find-minimals (+ edge-idx 1) first-free))
                   ; if closer=>register the edge
                   (else (if (edge< edge (vector-ref selected-edges from-set))
                             (vector-set! selected-edges from-set edge)) 
                         (if (edge< edge (vector-ref selected-edges to-set))
                             (vector-set! selected-edges to-set edge))
                         ; store it in first-free
                         (vector-set! edges first-free edge) 
                         (find-minimals (+ edge-idx 1) (+ first-free 1)))))
           first-free))) 
   (define tree '())
   (do ((remaining-edges (select-edges n-e) (select-edges remaining-edges)))
     ((= remaining-edges 0) tree)
     (vector-map!
      selected-edges
      (lambda (idx edge)
        (define mst-1 (dset:find node-sets (car edge)))
        (define mst-2 (dset:find node-sets (cddr edge)))
        (when (and (not (eq? edge infty-edge))
                   (not (dset:same-set? mst-1 mst-2)))
          ; mst-1 and mst-2 can be the same because of this very loop
          (dset:union! node-sets mst-1 mst-2)
          (set! tree (cons edge tree)))
        infty-edge))))

;;Test Vraag 4
(display "MST van graf cormen571split, gevonden door algoritme van Boruvka: ")(newline)
(display (mst-boruvka cormen571split))(newline)
(newline)

;;Output Vraag 4
;;MST van graf cormen571split, gevonden door algoritme van Boruvka:
;;{{7 0 8 . 7} {7 6 6 . 8} {7 6 1 . 7} {5 3 9 . 4} {5 2 7 . 3} {5 2 4 . 5} {7 0 4 . 1}}