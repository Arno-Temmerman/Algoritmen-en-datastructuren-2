#lang r7rs

;; Algoritmen en datastructuren II
;; Vraag 2 - DFT & BFT als routeplanner

;; a) Adjacency list representatie is het meest geschikt voor het opslaan van
;;    wegennetwerken omdat een wegnetwerk een ijle graaf vormt. Slechts enkele
;;    steden zijn rechtstreeks met elkaar verbonden via een snelweg.
;;    Bij deze representatie is het gebruikte geheugen voor het voorstellen van de bogen
;;    evenredig met het aantal effectief bestaande bogen. Bij de adjacency
;;    matrix representatie is dit geheugengebruik altijd evenredig aan het 
;;    maximaal aantal mogelijke knopen omdat ook "niet-bogen" opgeslagen worden.

(import (scheme base)    
        (scheme write)
        (a-d graph labeled config)
        (except (a-d graph-traversing dft-labeled)
                root-nop node-nop edge-nop)
        (a-d graph-traversing bft-labeled)
        (a-d graph examples undirected-labeled-unweighted)) 

(define (routeplanner-dft g start goal)
  (define route '())
  ;; Ga diepte eerst door de graaf en
  ;; geef een route van start naar goal
  (dft g
       root-nop ;; root-discovered
       (lambda (node node-label) ;; node-discovered
         (not (eq? node goal)))  ;; stopt op #f
       node-nop ;; node-processed
       edge-nop ;; edge-discovered
       edge-nop ;; edge-processed
       edge-nop ;; edge-bumped
       (list start))) ;; roots
       

;;Testen:
(display "DFT:")(newline)
(routeplanner-dft highways-belgium Oostende Aarlen)(newline)
(routeplanner-dft highways-belgium La_Louviere Zeebrugge)(newline)
(newline)


(define (routeplanner-bft g start goal)
  (define parent-vector (make-vector (order g) #f))
  ;; Ga breedte-eerst door de graaf en
  ;; geef een route van start naar goal
  (bft g
       node-nop ;; root-discovered
       node-nop ;; node-processed
       edge-nop ;; edge-discovered
       edge-nop ;; edge-numped
       (list start))) ;; roots


;;Testen:
(display "BFT:")(newline)
(routeplanner-bft highways-belgium Oostende Aarlen)(newline)
(routeplanner-bft highways-belgium La_Louviere Zeebrugge)(newline)
(newline)
