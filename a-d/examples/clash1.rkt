#lang r7rs

(define-library (box1)
  (import (scheme base))
  (export f)

  (begin
    (define (f x y)
      (* x y))))
