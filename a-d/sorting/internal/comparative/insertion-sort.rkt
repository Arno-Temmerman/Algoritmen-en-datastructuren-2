;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Insertion Sort                          *-*-
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
 
    (define (insertion-sort vector <<?)
      (define (>=? x y) (not (<<? x y)))
      (let outer-loop
        ((outer-idx (- (vector-length vector) 2)))
        (let
            ((current (vector-ref vector outer-idx)))
          (vector-set! 
           vector 
           (let inner-loop
             ((inner-idx (+ 1 outer-idx)))
             (cond
               ((or (>= inner-idx (vector-length vector))
                    (>=? (vector-ref vector inner-idx)
                         current))
                (- inner-idx 1))
               (else
                (vector-set! vector (- inner-idx 1) (vector-ref vector inner-idx))
                (inner-loop (+ inner-idx 1)))))
           current)
          (if (> outer-idx 0)
              (outer-loop (- outer-idx 1))))))
 
    (define sort insertion-sort)))