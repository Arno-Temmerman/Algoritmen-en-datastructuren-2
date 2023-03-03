#lang r7rs

(define-library (box2)
  (import (scheme base))
  (export f)

  (begin
    (define (f x)
      (* x x))))