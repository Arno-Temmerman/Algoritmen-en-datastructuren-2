#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Longest Increasing Subsequence (Naive)              *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2014  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (lis-native)
  (export lis)
  (import (scheme base))
  (begin
 
    (define (lis vect) ; longest increasing subsequence
      (define res (make-vector (vector-length vect) 0))
      (define (L j)
        (if (= j 0)
            0
            (+ 1 (let loop
                   ((i 0)
                    (max 0))
                   (if (= i (vector-length res))
                       max
                       (let ((Li (L i)))
                         (if (< Li max)
                             (loop (+ i 1) max)
                             (loop (+ i 1) Li))))))))
      (do ((i 0 (+ i 1)))
        ((= i (vector-length res)) res)
        (vector-set! res i (L i))))))