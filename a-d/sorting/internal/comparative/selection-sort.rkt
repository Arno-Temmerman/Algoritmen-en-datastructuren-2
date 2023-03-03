;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Selection Sort                          *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r7rs

(define-library (sorting)
  (export sort)
  (import (scheme base))
  (begin
    
    (define (selection-sort vector <<?)
      (define (swap vector i j)
        (let ((keep (vector-ref vector i)))
          (vector-set! vector i (vector-ref vector j))
          (vector-set! vector j keep)))
      (let outer-loop
        ((outer-idx 0))
        (swap vector
              outer-idx 
              (let inner-loop
                ((inner-idx (+ outer-idx 1))
                 (smallest-idx outer-idx))
                (cond 
                  ((>= inner-idx (vector-length vector))
                   smallest-idx)
                  ((<<? (vector-ref vector inner-idx)
                        (vector-ref vector smallest-idx))
                   (inner-loop (+ inner-idx 1) inner-idx))
                  (else
                   (inner-loop (+ inner-idx 1) smallest-idx)))))
        (if (< outer-idx (- (vector-length vector) 1))
            (outer-loop (+ outer-idx 1)))))
 
    (define sort selection-sort)))