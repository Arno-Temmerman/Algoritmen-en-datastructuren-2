#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d scheme-tools)
        (a-d graph weighted config)
        ;voor originele floyd-warshall (om te vergelijken in de tests)
        (a-d graph-algorithms directed traclo-weighted)
        (a-d graph examples directed-weighted))

;; TODO: Laat het algoritme van Floyd-Warshall ook een 2e matrix teruggeven die
;;       voor ieder paar knopen (from, to) zegt langs welke (andere) knoop (via) 
;;       je moet geraken vanuit 'from' om het pad tot 'to' te krijgen
;;       m.a.w. ALS inhoud op index (from, to) = via DAN from -> ... -> via -> to 

(define (floyd-warshall-with-tree g)
  (define traclo-distances               ; Matrix met alle afstanden/lengtes tussen iedere knoop.
    (make-2D-vector (order g)            ; Initieel is dit eigenlijk de adjacency matrix van de graaf.
                    (order g)            ; Deze matrix wordt iteratief aangepast om de transitieve sluiting te bekomen,
                    (lambda (i j)        ; d.i. een matrix die de lengtes tussen alle paren van knopen in de graaf bevat.
                      (weight g i j))))

  (define traclo-paths
    (make-2D-vector (order g)
                    (order g)
                    (lambda (i j)
                      (if (adjacent? g i j)
                          i
                          '()))))
  (for-each-node
   g
   (lambda (via)
     (for-each-node
      g
      (lambda (from)
        (for-each-node
         g
         (lambda (to)
           (ij!
            traclo-paths
            from to (if (<= (ij? traclo-distances from to)
                            (+ (ij? traclo-distances from via)
                               (ij? traclo-distances via to)))
                        (ij? traclo-paths from to)
                        (ij? traclo-paths via to)))
           (ij!
            traclo-distances
            from to (min (ij? traclo-distances from to)
                         (+ (ij? traclo-distances from via)
                            (ij? traclo-distances via to))))))))))
  ;traclo-distances
  (cons traclo-distances traclo-paths))

;;TESTS
(display (list "Floyd-Warshall " (floyd-warshall           cormen)))(newline)
;; traclo-distances
; #(#(4.0  1.0 -3.0  2.0 -4.0)
;   #(3.0  0.0 -4.0  1.0 -1.0)
;   #(7.0  4.0  0.0  5.0  3.0)
;   #(2.0 -1.0 -5.0  0.0 -2.0)
;   #(8.0  5.0  1.0  6.0  4.0))


(display (list "Floyd-Warshall with tree " (floyd-warshall-with-tree cormen)))(newline)
;; traclo-distances
;{#(#(4.0  1.0 -3.0  2.0 -4.0)
;   #(3.0  0.0 -4.0  1.0 -1.0)
;   #(7.0  4.0  0.0  5.0  3.0)
;   #(2.0 -1.0 -5.0  0.0 -2.0)
;   #(8.0  5.0  1.0  6.0  4.0))

;; traclo-paths
; #(#(3 2 3 4 0)
;   #(3 2 3 1 0)
;   #(3 2 3 1 0)
;   #(3 2 3 1 0)
;   #(3 2 3 4 0)))

;; Belangrijk om te weten hoe je deze matrix moet lezen!
;; De cormen graaf wordt hier onder getoond als voorbeeld.

;; Bijvoorbeeld: we zoeken het pad van 0 naar 1.
;; Lees index (0,1) --> hier vinden we 2
;; dus, om aan 1 te geraken vanuit 0 moeten we aan 2 geraken
;;   om te zien hoe we van 0 naar 2 geraken, lezen we index (0,2)
;;   hier vinden we 3
;;   dus, om aan 2 te geraken vanuit 0 moeten we aan 3 geraken
;;     om te zien hoe we van 0 naar 3 geraken, lezen we index (0,3)
;;     hier vinden we 4
;;     dus, om aan 3 te geraken vanuit 0 moeten we aan 4 geraken
;;         om te zien hoe we van 0 naar 4 geraken, lezen we index (0,4)
;;         hier vinden we 0
;;         dit wil zeggen dat de korste weg van 0 naar 4 een rechtstreekse pijl is
;; Met deze operaties kunnen we het goedkoopste pad van 0 naar 1 reconstrueren:
;; 1 <--(4)-- 2 <--(-5)-- 3 <--(6)-- 4 <--(-4)-- 0


(define (path-from-traclo g traclo i j)
  (let loop ((next j))
    (let ((prev (ij? traclo i next)))
      (unless (null? prev)
        (display next)
        (display " <--(")
        (display (weight g prev next))
        (display ")-- ")
        (if (eq? prev i)
            (display prev)
            (loop prev))))))


(display "Path from 0 to 1 given Floyd-Warshall traclo: ")
(path-from-traclo cormen (cdr (floyd-warshall-with-tree cormen)) 0 1)
;; 1 <--(4)-- 2 <--(-5)-- 3 <--(6)-- 4 <--(-4)-- 0
