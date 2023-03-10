#lang r7rs

;; Algoritmen en datastructuren II
;; Vraag 1 - Gelabelde grafen aan de hand van adjacency lists

;; Test cases

(import (a-d graph labeled config) ; Plaats je oplossing in a-d/graph/labeled voor later gebruik
        (scheme base)
        (scheme write))

(define a (new #t 5))

(label! a 0 "nul")
(label! a 1 "een")
(label! a 2 "twee")
(label! a 3 "drie")
(label! a 4 "vier")

(add-edge! a 0 1 "een")
(add-edge! a 2 3 "vijf")
(add-edge! a 2 4 "zes")
(add-edge! a 3 4 "zeven")

(for-each-node a (lambda (node node-label) 
                   (for-each-edge a node (lambda (to edge-label)
                                           (display node-label)
                                           (display " plus ")
                                           (display (label a to))
                                           (display " is ")
                                           (display edge-label)
                                           (newline)))))