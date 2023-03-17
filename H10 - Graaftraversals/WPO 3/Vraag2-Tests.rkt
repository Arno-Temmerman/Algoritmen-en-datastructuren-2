#lang r7rs

;; Algoritmen en datastructuren II
;; Vraag 2 - DFT & BFT als routeplanner

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
