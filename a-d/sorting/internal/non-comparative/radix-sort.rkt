;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                          Radix Sort                             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r7rs

(define-library (sorting)
  (export radix-sort)
  (import (scheme base))
  (begin
    
    (define (radix-sort slst key key-size)
      (define sort-bins (make-vector 10 '()))
      (define (spread lst digit-k)
        (define (digit item) 
          (remainder (quotient item (expt 10 digit-k)) 10))
        (define (spread-iter lst)
          (let ((idx (digit (key (car lst)))))
            (vector-set! sort-bins 
                         idx 
                         (cons (car lst)
                               (vector-ref sort-bins idx)))
            (if (not (null? (cdr lst)))
                (spread-iter (cdr lst)))))
        (spread-iter lst))
      (define (collect)
        (define (collect-iter index acc)
          (define (collect-list lst acc)
            (if (null? lst)
                (if (> index 0)
                    (collect-iter (- index 1) acc)
                    acc)
                (collect-list (cdr lst) (cons (car lst) acc))))
          (let ((l-index (vector-ref sort-bins index)))
            (vector-set! sort-bins index '())
            (collect-list l-index acc)))
        (collect-iter 9 '()))
      (define (radix-iter digit-k slst)
        (spread slst digit-k)
        (if (= digit-k key-size)
            (collect)
            (radix-iter (+ 1 digit-k) (collect))))
      (radix-iter 0 slst))))