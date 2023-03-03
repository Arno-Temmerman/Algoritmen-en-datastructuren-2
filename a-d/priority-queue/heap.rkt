#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Priority Queues (Heap Implementation)              *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (priority-queue)
  (export new priority-queue? enqueue! peek serve! full? empty?)
  (import (scheme base)
          (prefix (a-d heap standard) heap:))

  (begin
    (define pq-item-make cons)
    (define pq-item-val car)
    (define pq-item-priority cdr)
    (define (lift func)
      (lambda (item1 item2)
        (func (pq-item-priority item1)
              (pq-item-priority item2))))
 
    (define-record-type priority-queue
      (make h)
      priority-queue?
      (h heap))

    (define default-size 50)
  
    (define (new >>?)
      (make (heap:new default-size (lift >>?))))

    (define (empty? pq)
      (heap:empty? (heap pq)))
 
    (define (full? pq)
      (heap:full? (heap pq)))
 
    (define (serve! pq)
      (if (empty? pq)
          (error "empty priority queue (serve!)" pq)
          (pq-item-val (heap:delete! (heap pq)))))
 
    (define (peek pq)
      (if (empty? pq)
          (error "empty priority queue (peek)" pq)
          (pq-item-val (heap:peek (heap pq)))))
 
    (define (enqueue! pq value pty)
      (heap:insert! (heap pq) (pq-item-make value pty))
      pq)))