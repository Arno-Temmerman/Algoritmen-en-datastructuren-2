#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*         Pattern Matching (Knuth Morris Pratt Algorithm)         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (kmp) 
  (export match)
  (import (scheme base))
  (import (only (scheme write) display write))

  (begin
    (define (compute-failure-function p)
      (define n-p (string-length p))
      (define sigma-table (make-vector n-p 0))
      (let loop
        ((i-p 2)
         (k 0))
        (when (< i-p n-p)
          (cond
            ((eq? (string-ref p k) 
                  (string-ref p (- i-p 1)))
             (vector-set! sigma-table i-p (+ k 1))
             (loop (+ i-p 1) (+ k 1)))
            ((> k 0)
             (loop i-p (vector-ref sigma-table k)))
            (else ; k=0
             (vector-set! sigma-table i-p 0)
             (loop (+ i-p 1) k)))))
        (vector-set! sigma-table 0 -1)
        (lambda (q)
          (vector-ref sigma-table q)))
 
    (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (define sigma (compute-failure-function p))
      (let loop
        ((i-t 0)
         (i-p 0)) 
        (cond 
          ((> i-p (- n-p 1))
           i-t)
          ((> i-t (- n-t n-p))
           #f)
          ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
           (loop i-t (+ i-p 1)))
          (else
           (loop (+ i-t (- i-p (sigma i-p))) (if (> i-p 0)
                                                 (sigma i-p)
                                                 0))))))))