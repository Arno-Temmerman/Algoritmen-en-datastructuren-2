#lang r7rs

;; Algoritmen en datastructuren II
;; Vraag 2 - Connectiviteit van grafen testen met disjoint sets

;; Test cases

(import (a-d graph unweighted 2-adjacency-matrix) ;; Importeer hier je oplossing
        (scheme base)
        (scheme write))

;figuur a en b uit de opgave
(define a (new #f 7))

(add-edge! a 0 2)
(add-edge! a 1 2)
(add-edge! a 2 3)
(add-edge! a 4 5)
(add-edge! a 5 6)

(define b (new #f 7))

(add-edge! b 0 2)
(add-edge! b 1 2)
(add-edge! b 2 3)
(add-edge! b 4 5)
(add-edge! b 5 6)
(add-edge! b 2 5)
(add-edge! b 3 6)

(display "a connected? ")
(display (connected? a))
(newline)
(display "in a connection between 0 1?")
(display (connection? a 0 1))
(newline)
(display "in a connection between 2 5?")
(display (connection? a 2 5))
(newline)

(display "b connected? ")
(display (connected? b))
(newline)
(display "in b connection between 0 1?")
(display (connection? b 0 1))
(newline)
(display "in b connection between 2 5?")
(display (connection? b 2 5))
(newline)