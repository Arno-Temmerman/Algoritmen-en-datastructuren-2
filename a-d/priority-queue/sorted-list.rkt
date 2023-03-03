#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*          Priority Queues (Sorted List Implementation)           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (priority-queue)
  (export new priority-queue? enqueue! serve! peek full? empty?)
  (import (except (scheme base) list)
          (prefix (a-d sorted-list linked) slist:))
  (begin
    (define pq-item-make cons)
    (define pq-item-val car)
    (define pq-item-priority cdr)
    (define (lift func)
      (lambda (item1 item2)
        (func (pq-item-priority item1)
              (pq-item-priority item2))))
 
    (define-record-type priority-queue
      (make s)
      priority-queue?
      (s slist))
 
    (define (new >>?)
      (make (slist:new (lift >>?)
                       (lift eq?))))
 
    (define (empty? pq)
      (slist:empty? (slist pq)))
 
    (define (full? pq)
      (slist:full? (slist pq)))
 
    (define (enqueue! pq val pty)
      (slist:add! (slist pq) (pq-item-make val pty))
      pq)
 
    (define (serve! pq)
      (define slst (slist pq))
      (if (empty? pq)
          (error "empty priority queue (serve!)" pq))
      (slist:set-current-to-first! slst)
      (let ((served-item (slist:peek slst)))
        (slist:delete! slst)
        (pq-item-val served-item)))
 
    (define (peek pq)
      (define slst (slist pq))
      (if (empty? pq)
          (error "empty priority queue (peek)" pq))
      (slist:set-current-to-first! slst)
      (pq-item-val (slist:peek slst)))))