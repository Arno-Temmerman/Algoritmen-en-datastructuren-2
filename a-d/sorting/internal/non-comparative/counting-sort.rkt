;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Counting Sort                           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

#lang r7rs

(define-library (sorting)
  (export counting-sort)
  (import (scheme base))
  (begin
    
    (define (counting-sort in out max-key key)
      (let ((count (make-vector max-key 0))
            (size (vector-length in)))    
        (define (fill-count-vector i)
          (let ((k (key (vector-ref in i))))
            (vector-set! count
                         k (+ (vector-ref count k) 1))
            (if (< (+ i 1) size)
                (fill-count-vector (+ i 1)))))
        (define (sum-vector i)
          (vector-set! count 
                       i
                       (+ (vector-ref count (- i 1))
                          (vector-ref count i)))
          (if (< (+ i 1) max-key)
              (sum-vector (+ i 1))
              count))
        (define (spread-out-again i)
          (let* ((data (vector-ref in i))
                 (k (- (vector-ref count (key data)) 1)))
            (vector-set! out k data)
            (vector-set! count (key data) k)
            (if (<= i 0)
                out
                (spread-out-again (- i 1)))))
        (fill-count-vector 0)
        (sum-vector 1)
        (spread-out-again (- size 1))))))