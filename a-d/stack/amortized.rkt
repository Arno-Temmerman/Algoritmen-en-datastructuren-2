#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                stack (Amortized Implementation)                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2018  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (stack)
  (export new stack? push! pop! top empty? full?)
  (import (scheme base))
  (begin
    
    (define initial-size 1)
 
    (define-record-type stack
      (make f s)
      stack?
      (f first-free first-free!)
      (s storage storage!))
 
    (define (new)
      (make 0 (make-vector initial-size '())))
 
    (define (push! stck valu)
      (define vctr (storage stck))
      (define free (first-free stck))
      (define (extend-vector)
        (define new-vector (make-vector (* (vector-length vctr) 2) '()))
        (do ((i 0 (+ i 1)))
          ((= i free) new-vector)
          (vector-set! new-vector i (vector-ref vctr i))))
      (when (= free (vector-length vctr))
        (set! vctr (extend-vector))
        (storage! stck vctr))
      (vector-set! vctr free valu)
      (first-free! stck (+ free 1))
      stck)
 
    (define (top stck)
      (define vctr (storage stck))
      (define free (first-free stck))
      (if (= free 0)
          (error "stack empty (top)" stck))
      (vector-ref vctr (- free 1)))
 
    (define (pop! stck)
      (define vctr (storage stck))
      (define free (first-free stck))
      (if (= free 0)
          (error "stack empty (pop!)" stck))
      (let ((valu (vector-ref vctr (- free 1))))
        (first-free! stck (- free 1))
        valu))
 
    (define (empty? stck)
      (= (first-free stck) 0))
 
    (define (full? stck)
      #f)))