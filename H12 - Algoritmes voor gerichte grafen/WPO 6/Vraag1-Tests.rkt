#lang r7rs

(import (scheme base)
        (scheme write)
        (a-d graph unweighted config)
        (a-d graph-traversing dft-unweighted)
        (a-d graph examples directed-unweighted))

; Reeds geziene implementatie om te testen op cycli in ONgerichte grafen
; Origineel terug te vinden in a-d/graph-algorithms/undirected/dft-applications.rkt
(define (cyclic? g)  
  (define tree (make-vector (order g) '()))
  (define cyclic #f)
  (dft g
       root-nop                       ;root-discovered
       node-nop                       ;node-discovered
       node-nop                       ;node-processed
       (lambda (from to)              ;edge-discovered
         (vector-set! tree from to))
       edge-nop                       ;edge-processed
       (lambda (from to)              ;edge-bumped
         (when (not (eq? (vector-ref tree to) from)) ; test op echte terugbogen
           (set! cyclic #t)
           #f)))                      ;terminate the traversal once a cycle has been found
  cyclic)

;; TODO: Implementeer een versie van cyclic? dat zowel werkt voor gerichte
;; als ongerichte grafen
(define (cyclic*? g)
  (define tree (make-vector (order g) '()))
  (define processed (make-vector (order g) #f))
  (define cyclic #f)
  (dft g
       root-nop                       ;root-discovered
       node-nop                       ;node-discovered
       (lambda (node)
         (vector-set! processed node #t)) ;node-processed
       (lambda (from to)              ;edge-discovered
         (vector-set! tree from to))
       edge-nop                       ;edge-processed
       (lambda (from to)              ;edge-bumped
         (when (if (directed? g)
                   (not (vector-ref processed to))
                   (not (eq? (vector-ref tree to) from))) ; test op echte terugbogen
           (set! cyclic #t)
           #f)))                      ;terminate the traversal once a cycle has been found
  cyclic)

;; Voorbeeld grafen

(define two-node-cycle
  (let ((g (new #t 2)))
    (add-edge! g 0 1)
    (add-edge! g 1 0)
    g))

;; Tekening:
;;
;; 0 <-> 1


(define acyclic-directed-example
  (let ((g (new #t 3)))
    (add-edge! g 0 1)
    (add-edge! g 0 2)
    (add-edge! g 2 1)
    g))

;; Tekening:
;;
;;  0 --------> 1 
;;   \          ^
;;    \         |
;;     \--> 2 --/


(define acyclic-directed-example2
  (let ((g (new #t 3)))
    (add-edge! g 0 1)
    (add-edge! g 0 2)
    (add-edge! g 1 2)
    g))

;; Tekening:
;;
;;  0 ---------> 1
;;   \           |
;;    \          |
;;     \--> 2 <--/


(define cyclic-directed-example
  (let ((g (new #t 5)))
    (add-edge! g 0 1)
    (add-edge! g 0 2)
    (add-edge! g 2 3)
    (add-edge! g 3 4)
    (add-edge! g 4 2)
    (add-edge! g 4 1)
    g))

;; Tekening:
;;   0 ---------------------> 1 <--\   
;;   |                             |
;;   \--> 2 <-----------\          |    
;;        |             |          |
;;        \--> 3        |          |
;;             |        |          |
;;             \--> 4 __/          |
;;                  \______________/


(display "two-node-cycle met cylic?: ")
(display (cyclic? two-node-cycle))(newline)
(display "two-node-cycle met cyclic*?: ")
(display (cyclic*? two-node-cycle))(newline)
(newline)
(display "acyclic-directed-example met cylic?: ")
(display (cyclic? acyclic-directed-example))(newline)
(display "acyclic-directed-example met cyclic*?: ")
(display (cyclic*? acyclic-directed-example))(newline)
(newline)
(display "acyclic-directed-example2 met cylic?: ")
(display (cyclic? acyclic-directed-example2))(newline)
(display "acyclic-directed-example2 met cyclic*?: ")
(display (cyclic*? acyclic-directed-example2))(newline)
(newline)
(display "cyclic-directed-example met cylic?: ")
(display (cyclic? cyclic-directed-example))(newline)
(display "cyclic-directed-example met cyclic*?: ")
(display (cyclic*? cyclic-directed-example))(newline)