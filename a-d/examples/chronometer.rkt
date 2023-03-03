#lang r7rs

(define-library (chronometer)
  (import (scheme base)
          (srfi 19))
  (export time-it test fib)

  (begin

    (define (fib n)
      (if (< n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2)))))
    
    (define (sleep n)
      (if (> n 0)
          (sleep (- n 1))))

    (define (test n)
      (time-it (sleep n)))
 
    (define-syntax time-it
      (syntax-rules (expression)
        ((time-it! exp) (let* ((t1 (current-time))
                               (val exp)
                               (t2 (current-time)))
                          (cons (time-second (time-difference t2 t1))
                                (time-nanosecond (time-difference t2 t1)))))))))