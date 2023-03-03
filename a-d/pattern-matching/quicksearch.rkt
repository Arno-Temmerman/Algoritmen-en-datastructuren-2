#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Pattern Matching (Sunday QuickSearch)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (quicksearch)
  (export match)
  (import (scheme base))

  (begin
    (define (compute-shift-function p)
      (define n-p (string-length p))
      (define min-ascii (char->integer (string-ref p 0)))
      (define max-ascii min-ascii)
   
      (define (create-table index)
        (if (< index n-p)
            (begin
              (set! min-ascii (min min-ascii (char->integer (string-ref p index))))
              (set! max-ascii (max max-ascii (char->integer (string-ref p index))))
              (create-table (+ index 1)))
            (make-vector (- max-ascii min-ascii -1) (+ n-p 1)))) 
   
      (define (fill-table index)
        (if (< index n-p)
            (let ((ascii (char->integer (string-ref p index))))
              (vector-set! shift-table (- ascii min-ascii) (- n-p index))
              (fill-table (+ index 1)))))
   
      (define shift-table (create-table 0))
      (fill-table 0)
      (lambda (c)
        (let ((ascii (char->integer c)))
          (if (>= max-ascii ascii min-ascii)
              (vector-ref shift-table (- ascii min-ascii))
              (+ n-p 1)))))
 
    (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
      (define shift (compute-shift-function p))
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
           (let ((c-t (string-ref t (modulo (+ i-t n-p) n-t))))
             (loop (+ i-t (shift c-t)) 0))))))))