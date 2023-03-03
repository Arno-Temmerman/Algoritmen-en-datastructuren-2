#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Queues (Circular Vector)                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (queue)
  (export new queue? enqueue! serve! peek full? empty?)
  (import (scheme base))
  (begin
   
    (define default-size 5)
    (define-record-type queue
      (make s h r)
      queue?
      (s storage)
      (h head head!)
      (r rear rear!))
 
    (define (new)
      (make (make-vector default-size) 0 0))
 
    (define (empty? q)
      (= (head q)
         (rear q)))
 
    (define (full? q)
      (= (remainder (+ (rear q) 1) default-size)
         (head q)))
 
    (define (enqueue! q val)
      (if (full? q)
          (error "full queue (enqueue!)" q))
      (let ((new-rear (remainder (+ (rear q) 1) default-size)))
        (vector-set! (storage q) (rear q) val)
        (rear! q new-rear))
      q)
 
    (define (peek q)
      (if (empty? q)
          (error "empty queue (peek)" q))
      (vector-ref (storage q) (head q)))
 
    (define (serve! q)
      (if (empty? q)
          (error "empty queue (peek)" q))
      (let ((result (vector-ref (storage q) (head q))))
        (head! q (remainder (+ (head q) 1) default-size))
        result))))