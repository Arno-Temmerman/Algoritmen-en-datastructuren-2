;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                           Bubble Sort                           *-*-
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
    
    (define (bubble-sort vector <<?)  
      (define (bubble-swap vector idx1 idx2)
        (let ((keep (vector-ref vector idx1)))
          (vector-set! vector idx1 (vector-ref vector idx2))
          (vector-set! vector idx2 keep)
          #t))
      (let outer-loop
        ((unsorted-idx (- (vector-length vector) 2)))
        (if (>= unsorted-idx 0)
            (if (let inner-loop
                  ((inner-idx 0)
                   (has-changed? #f))
                  (if (> inner-idx unsorted-idx)
                      has-changed?
                      (inner-loop (+ inner-idx 1)
                                  (if (<<? (vector-ref vector (+ inner-idx 1))
                                           (vector-ref vector inner-idx))
                                      (bubble-swap vector inner-idx (+ inner-idx 1))
                                      has-changed?))))
                (outer-loop (- unsorted-idx 1))))))
 
    (define sort bubble-sort)))