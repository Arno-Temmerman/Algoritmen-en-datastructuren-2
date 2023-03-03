#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Pattern Matching (Horspool Algorithm)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2007 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (horspool)
  (export match)
  (import (scheme base))

  (begin
    (define (compute-shift-table p)
      (define n-p (string-length p))
      (define min-ascii (char->integer (string-ref p 0)))
      (define max-ascii min-ascii)
   
      (define (calculate-table index)
        (if (< index n-p)
            (begin
              (set! min-ascii (min min-ascii (char->integer (string-ref p index))))
              (set! max-ascii (max max-ascii (char->integer (string-ref p index))))
              (calculate-table (+ index 1)))
            (make-vector (- max-ascii min-ascii -1) n-p)))
   
      (define (traverse index)
        (if (< (+ index 1) n-p)
            (let ((ascii (char->integer (string-ref p index))))
              (vector-set! jump-table (- ascii min-ascii) (- n-p 1 index))
              (traverse (+ index 1)))))
   
      (define jump-table (calculate-table 0))
      (traverse 0)
      (lambda (c)
        (let ((ascii (char->integer c)))
          (if (>= max-ascii ascii min-ascii)
              (vector-ref jump-table (- ascii min-ascii))
              n-p))))
 
    (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (define shift (compute-shift-table p))
      (let loop
        ((i-t 0)
         (i-p (- n-p 1)))
        (cond 
          ((< i-p 0)
           i-t)
          ((> i-t (- n-t n-p))
           #f)
          ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
           (loop i-t (- i-p 1)))
          (else
           (loop (+ i-t (shift (string-ref t (+ i-t n-p -1))))
                 (- n-p 1))))))))