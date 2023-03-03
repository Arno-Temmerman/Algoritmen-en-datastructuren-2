#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Basic Undirected Graph Algorithms                *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (basic)
  (export degrees connected?)
  (import (scheme base)
          (a-d graph unweighted config))
  (begin
   
    (define (degrees g)
      (define degs (make-vector (order g) 0))
      (for-each-node
       g
       (lambda (from)
         (for-each-edge
          g
          from
          (lambda (to)
            (vector-set! degs from (+ 1 (vector-ref degs from)))))))
      degs)

    (define (connected? g node1 node2)
      (define visited (make-vector (order g) #f))
      (call-with-current-continuation
       (lambda (return)
         (define (connected-rec x y)
           (if (eq? x y)
               (return #t))
           (vector-set! visited x #t)
           (for-each-edge g
                          x
                          (lambda (t)
                            (if (not (vector-ref visited t))
                                (connected-rec t y)))))
         (connected-rec node1 node2)
         #f)))))