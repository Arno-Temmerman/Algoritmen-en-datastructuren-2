#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                stack (Vectorial Implementation)                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


(define-library (stack)
  (export new stack? push! pop! top empty? full?)
  (import (scheme base))
  (begin
 
    (define stack-size 10)

    (define-record-type stack
      (make f v)
      stack?
      (f first-free first-free!)
      (v storage))
 
    (define (new)
      (make 0 (make-vector stack-size '())))
 
    (define (push! stack val)
      (define vector (storage stack))
      (define ff (first-free stack))
      (if (= ff (vector-length vector))
          (error "stack full (push!)" stack))
      (vector-set! vector ff val)
      (first-free! stack (+ ff 1))
      stack)
 
    (define (top stack)
      (define vector (storage stack))
      (define ff (first-free stack))
      (if (= ff 0)
          (error "stack empty (top)" stack))
      (vector-ref vector (- ff 1)))
 
    (define (pop! stack)
      (define vector (storage stack))
      (define ff (first-free stack))
      (if (= ff 0)
          (error "stack empty (pop!)" stack))
      (let ((val (vector-ref vector (- ff 1))))
        (first-free! stack (- ff 1))
        val))
 
    (define (empty? stack)
      (= (first-free stack) 0))
 
    (define (full? stack)
      (= (first-free stack)
         (vector-length (storage stack))))))