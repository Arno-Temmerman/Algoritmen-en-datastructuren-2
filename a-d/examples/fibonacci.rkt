#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   2 Fibonacci Implementations                   *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(import (scheme base)
        (scheme write))

(define (fib1 n)
  (if (< n 2)
    1
    (+ (fib1 (- n 1)) (fib1 (- n 2)))))

(define (fib2 n)
  (define (iter n a b)
    (if (= n 0)
      b
      (iter (- n 1) b (+ a b))))
  (iter n 0 1))

(define (triangle n)
  (define (line i)
    (display "*")
    (if (= i 0)
        (newline)
      (line (- i 1))))
  (line n)
  (if (> n 0)
    (triangle (- n 1))))

(define (sum-three-to-the n)
  (define (power k)
    (if (= k 0)
      1
      (* 3 (power (- k 1)))))
  (if (= n 0)
    1
    (+ (power n)
       (sum-three-to-the (- n 1))))) 