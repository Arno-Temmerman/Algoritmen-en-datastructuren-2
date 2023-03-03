#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Pattern Matching (Brute Force Algorithm)             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (brute-foce)
  (export match)
  (import (scheme base))

  (begin
    (define (match t p)
      (define n-t (string-length t))
      (define n-p (string-length p))
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
           (loop (+ i-t 1) 0)))))))