#lang r7rs

;; Algoritmen en datastructuren II
;; Vraag 1 - Ouder-kind-relaties in DFT- & BFT-bossen
(import (scheme base)
        (scheme write)
        (a-d graph unweighted config)
        (except (a-d graph-traversing dft-unweighted) 
                root-nop node-nop edge-nop)
        (a-d graph-traversing bft)
        (a-d graph examples undirected-unweighted))

;; a) DFT versie (met parent-list):

(define (dft-parents g)
  ;; Ga diepte eerst door de graaf
  ;; en print voor elke knoop het pad
  ;; van die knoop tot de wortel
  (define parent-stack '())
  (dft g
       root-nop ;; root-discovered
       (lambda (node)
         (display "Parents van node ")(display node)(display ": ")(display parent-stack) (newline)
         (set! parent-stack (cons node parent-stack))) ;; node-discovered
       ; stack push from
       (lambda (node)
         (set! parent-stack (cdr parent-stack))) ;; node-processed
       ;; stack pop
       edge-nop ;; edge-discovered
       edge-nop ;; edge-processed
       edge-nop ;; edge-bumped
       ;; (roots)
       ))

;;  Tests:
(display "connected met DFT:")(newline)
(display "------------------")(newline)
(dft-parents connected)(newline)

(display "three-cc met DFT:")(newline)
(display "-----------------")(newline)
(dft-parents three-cc)(newline)

(display "kite met DFT:")(newline)
(display "-------------")(newline)
(dft-parents kite)(newline)

;; b) BFT versie (met parent-vector):

(define (bft-parents g)
  ;; Ga breedte eerste door de graaf
  ;; en print voor elke knoop het pad 
  ;; van die knoop tot de wortel
  (define parent-vector (make-vector (order g) #f))

  (define (tracer current-node)
    (define (display-parent-iter node)
      (let ((parent (vector-ref parent-vector node)))
        (when parent
          (display parent)
          (when (vector-ref parent-vector parent)
            (display " ")
            (display-parent-iter parent)))))
    (display "Parents van node ")(display current-node)(display ": ")
    (display-parent-iter current-node)(newline))

  (bft g
       root-nop ;; root-discovered
       (lambda (node)
         (tracer node)) ;; node-discovered
       (lambda (from to)
         (vector-set! parent-vector to from)) ;; edge-discovered
       edge-nop ;; edge-bumped
       ;; (roots)
       ))

;;  Tests:
(display "connected met BFT:")(newline)
(display "------------------")(newline)
(bft-parents connected)(newline)

(display "three-cc met BFT:")(newline)
(display "-----------------")(newline)
(bft-parents three-cc)(newline)

(display "kite met BFT:")(newline)
(display "-------------")(newline)
(bft-parents kite)(newline)
