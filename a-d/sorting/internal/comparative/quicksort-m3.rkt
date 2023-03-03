;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Quicksort (median of 3 version)                 *-*-
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
    
    (define (quicksort vector <<?)
      (define (swap i j)
        (let ((keep (vector-ref vector i)))
          (vector-set! vector i (vector-ref vector j))
          (vector-set! vector j keep)))
      (define (shift-to-right i x)
        (if (<<? (vector-ref vector i) x)
            (shift-to-right (+ i 1) x)
            i))
      (define (shift-to-left j x)
        (if (<<? x (vector-ref vector j))
            (shift-to-left (- j 1) x)
            j))
      (define (partition pivot i j)
        (let ((shifted-i (shift-to-right i pivot))
              (shifted-j (shift-to-left j pivot)))
          (cond ((< shifted-i shifted-j)
                 (swap shifted-i shifted-j)
                 (partition pivot shifted-i (- shifted-j 1)))
                (else
                 shifted-j))))
      (define (m3-partition l r)
        (let ((middle (quotient (+ l r) 2)))
          (swap middle (+ l 1))
          (if(<<? (vector-ref vector l)
                  (vector-ref vector (+ l 1)))
             (swap l (+ l 1)))
          (if (<<? (vector-ref vector r)
                   (vector-ref vector (+ l 1)))
              (swap r (+ l 1)))
          (if (<<? (vector-ref vector r)
                   (vector-ref vector l))
              (swap l r))
          (partition (vector-ref vector l) (+ l 1) (- r 1))))
      (define (quicksort-main vector l r)
        (if (< l r)
            (let ((m (m3-partition l r)))
              (swap l m)
              (quicksort-main vector l (- m 1))
              (quicksort-main vector (+ m 1) r))))
      (quicksort-main vector 0 (- (vector-length vector) 1)))
 
    (define sort quicksort)) )