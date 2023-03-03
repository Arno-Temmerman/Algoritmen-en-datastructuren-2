;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                            Mergesort                            *-*-
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
    
    (define (mergesort vector <<?)
      (define (merge vector p q r)
        (let ((working-vector (make-vector (+ (- r p) 1))))
          (define (copy-back a b)
            (vector-set! vector b (vector-ref working-vector a))
            (if (< a (- (vector-length working-vector) 1))
                (copy-back (+ a 1) (+ b 1))))
          (define (flush-remaining k i until)
            (vector-set! working-vector k (vector-ref vector i))
            (if (< i until)
                (flush-remaining (+ k 1) (+ i 1) until)
                (copy-back 0 p)))
          (define (merge-iter k i j)
            (cond ((and (<= i q) (<= j r))
                   (let ((low1 (vector-ref vector i))
                         (low2 (vector-ref vector j)))
                     (if (<<? low1 low2)
                         (begin 
                           (vector-set! working-vector k low1)
                           (merge-iter (+ k 1) (+ i 1) j))
                         (begin 
                           (vector-set! working-vector k low2)
                           (merge-iter (+ k 1) i (+ j 1))))))
                  ((<= i q)
                   (flush-remaining k i q))
                  (else
                   (flush-remaining k j r))))
          (merge-iter 0 p (+ q 1))))
      (define (merge-sort-rec vector p r)
        (if (< p r)
            (let ((q (quotient (+ r p) 2)))
              (merge-sort-rec vector p q)
              (merge-sort-rec vector (+ q 1) r)
              (merge vector p q r))))
      (merge-sort-rec vector 0 (- (vector-length vector) 1))
      vector)
 
    (define sort mergesort)))