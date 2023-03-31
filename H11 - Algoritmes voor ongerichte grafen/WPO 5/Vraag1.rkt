#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d graph weighted config)
        (a-d graph examples undirected-weighted)
        (rename (a-d sorting internal comparative quicksort) (sort quicksort))
        (a-d scheme-tools)
        (prefix (a-d priority-queue modifiable-heap) pq:) ; uitgebreid PQ ADT dat toelaat prioriteiten aan te passen
        (prefix (a-d disjoint-sets optimized) dset:))

;;Hulpprocedure om alle bogen van de graaf terug te geven in een vector
; Bogen zijn triplets {from . {weight . to}}
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

;; Pas de uitvoer van de procedure aan.

(define (mst-kruskal g)
  (define edges (all-edges g))
  (define n-e (vector-length edges))
  (define forest (new #f (order g)))
  (define node-sets (dset:new (order g))) ;; disjoint set ADT om cycli na te gaan.
  (quicksort edges (lambda (edge1 edge2) (< (cadr edge1) (cadr edge2)))) ; sorteer alle bogen van de graaf volgens gewicht
  (do ((edge-idx 0 (+ edge-idx 1)))
    ((= edge-idx n-e) forest)
    (let* ((edge (vector-ref edges edge-idx))
           (from (car edge))
           (weight (cadr edge))
           (to (cddr edge))
           (from-set (dset:find node-sets from))
           (to-set (dset:find node-sets to)))
      (when (not (dset:same-set? from-set to-set)) ; boog vormt geen cyclus
        (add-edge! forest from to weight) ; voeg boog toe aan het bos
        (dset:union! node-sets from-set to-set)))))


;;Test Kruskal Vraag 1
(display "MST van graf cormen571split, gevonden door algoritme van Kruskal: ")(newline)
(display (mst-kruskal cormen571split))(newline)
(display "Edges in MST graf cormen571split ")(newline)
(display (all-edges (mst-kruskal cormen571split)))(newline) 
(newline)

;;Output Kruskal Vraag 1
;;MST van graf cormen571split, gevonden door algoritme van Kruskal: #<weighted-graph>
;;Edges in MST graf cormen571split:
;;#({0 4 . 1} {0 8 . 7} {2 7 . 3} {2 4 . 5} {3 9 . 4} {6 1 . 7} {6 6 . 8})


;; Pas de uitvoer van de procedure aan.

(define (mst-prim-jarnik g)
  (define tree (new #f (order g)))
  (define pq-ids (make-vector (order g) '())) ;; hou voor iedere knoop zijn "positie" in de priority queue bij
  (define (track-ids id node weight) 
    (vector-set! pq-ids node id))
  (define (id-of node)
    (vector-ref pq-ids node))
  (define (pty< edge1 edge2) 
    (< (cdr edge1) (cdr edge2)))

  ; Hulpprocedure
  (define (degree g node)
    (define d 0)
    (for-each-edge g
                   node
                   (lambda (to weight)
                     (set! d (+ d 1))))
    d)

  ; priority queue van nodes (items) uit de graaf die we in het bos willen opnemen
  ; De prioriteit is een cons-cel.
  ; De car bevat een rechtstreekse buur van de knoop die reeds in het bos zit.
  ; Initieel is de car leeg (het bos bevat nog geen knopen).
  ; De cdr bevat de "afstand van die knoop tot het reeds gemaakte spanningsbos".
  ; Initieel is die afstand oneindig als de knoop in de PQ nog geen buren in de MST heeft.
  ; Eenmaal een knoop (item) in het bos wordt opgenomen,
  ; worden afstanden naar buren van die knoop geüpdate in de PQ naar het gewicht van de boog (priorities)
  (define pq (pq:new (order g) pty<))
  
  (define (prim-jarnik-iter closest-node&edge)
    (define closest-node (car closest-node&edge)) ; to
    (define closest-edge (cdr closest-node&edge)) ; (from . weight)
    ; closest-node wordt opgenomen in het bos via de boog closest-edge met bijbehorend gewicht
    (if (not (null? (car closest-edge)))
        (add-edge! tree closest-node (car closest-edge) (cdr closest-edge)))
    ; Plaats onbezochte buren van closest-node in de PQ
    (for-each-edge
     g closest-node
     (lambda (weight to)
       (define edge-to-to (cons closest-node weight)) ; maak een nieuwe priority voor to
       (if (null? (id-of to))
           (pq:enqueue! pq to edge-to-to track-ids)
           (if (and (= (degree tree to) 0)
                    (pty< edge-to-to (pq:priority-of pq (id-of to))))
               (pq:reschedule! pq (id-of to) edge-to-to track-ids)))))
    (unless (pq:empty? pq)
      (prim-jarnik-iter (pq:serve! pq track-ids)))) ; serve! geeft een node en zijn prioriteit terug in een cons-cel

  (for-each-node
   g (lambda (node)
       (when (= (degree tree node) 0)
         (pq:enqueue! pq node (cons '() +inf.0) track-ids)
         (prim-jarnik-iter (pq:serve! pq track-ids))))) ; serve! geeft een node en zijn prioriteit terug in een cons-cel

  
  tree)
 


;;Test Prim-Jarník Vraag 1
(display "MST van graf cormen571split, gevonden door algoritme van Prim-Jarník: ")(newline)
(display (mst-prim-jarnik cormen571split))(newline)
(display "Edges in MST graf cormen571split ")(newline)
(display (all-edges (mst-prim-jarnik cormen571split)))(newline) 
(newline)

;;Output Prim-Jarník Vraag 1
;;MST van graf cormen571split, gevonden door algoritme van Prim-Jarník: #<weighted-graph>
;;Edges in MST graf cormen571split:
;;#({0 4 . 1} {0 8 . 7} {2 7 . 3} {2 4 . 5} {3 9 . 4} {6 1 . 7} {6 6 . 8})



;; Pas de uitvoer van de procedure aan.

(define (mst-boruvka g)
  (define n-e (nr-of-edges g))
  (define edges (all-edges g)) ; vector met alle bogen
  (define infty-edge (cons 0 (cons +inf.0 0))) ; bogen zijn triples (from . (weight . to))
  (define (edge< edge1 edge2)
    (< (cadr edge1) (cadr edge2)))

  ; vector die voor ieder component telkens de goedkoopste boog bewaart
  ; initieel is iedere knoop in het bos een apart component, vandaar een node-indexed vector.
  (define selected-edges (make-vector (order g) infty-edge)) ; initieel heeft iedere component (knoop) een boog met oneindig gewicht
  
  (define node-sets (dset:new (order g))) ; hou bij tot welke component iedere knoop in het "bos tot nu toe" behoort

  (define (select-edges remaining-edges)
    (let find-minimals ; zoek voor iedere component de boog met het kleinste gewicht
      ((edge-idx 0)
       (first-free 0)) ; teller die aangeeft hoeveel bogen werden gevonden om de componenten te doen groeien 
      (if (< edge-idx remaining-edges)
          (let* ((edge (vector-ref edges edge-idx))
                 (from (car edge))
                 (to   (cddr edge))
                 (from-set (dset:find node-sets from))
                 (to-set   (dset:find node-sets to)))
            (cond ((dset:same-set? from-set to-set) ; edge already in MST-forest
                   (find-minimals (+ edge-idx 1) first-free)) ; skip it
                  (else (if (edge< edge (vector-ref selected-edges from-set))
                            (vector-set! selected-edges from-set edge)) ; closer=>register the edge
                        (if (edge< edge (vector-ref selected-edges to-set))
                            (vector-set! selected-edges to-set edge))   ; dito
                        ; geskipte bogen worden overschreven
                        (vector-set! edges first-free edge) ; store it in first-free
                        (find-minimals (+ edge-idx 1) (+ first-free 1)))))
          first-free))) 
  (define tree (new #f (order g)))
  (do ((remaining-edges (select-edges n-e) (select-edges remaining-edges)))
    ((= remaining-edges 0) tree)
    (vector-map!
     selected-edges
     (lambda (idx edge)
       (define mst-1 (dset:find node-sets (car edge)))
       (define mst-2 (dset:find node-sets (cddr edge)))
       (when (and (not (eq? edge infty-edge))
                  (not (dset:same-set? mst-1 mst-2))) ; mst-1 and mst-2 can be the same because of this very loop
         (dset:union! node-sets mst-1 mst-2)
         (add-edge! tree (car edge) (cddr edge) (cadr edge)))
       infty-edge))))


;; Test Borůvka Vraag 1
(display "MST van graf cormen571split, gevonden door algoritme van Borůvka: ")(newline)
(display (mst-boruvka cormen571split))(newline)
(display "Edges in MST graf cormen571split ")(newline)
(display (all-edges (mst-boruvka cormen571split)))(newline) 
(newline)

;;Output Borůvka Vraag 1
;;MST van graf cormen571split, gevonden door algoritme van Borůvka: #<weighted-graph>
;;Edges in MST graf cormen571split:
;;#({0 4 . 1} {0 8 . 7} {2 7 . 3} {2 4 . 5} {3 9 . 4} {6 1 . 7} {6 6 . 8})


