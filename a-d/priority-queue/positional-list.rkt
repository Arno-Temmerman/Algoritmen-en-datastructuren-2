#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*        Priority Queues (Positional List Implementation)         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (priority-queue)
  (export new priority-queue? enqueue! peek serve! full? empty?)
  (import (except (scheme base) list)
          (prefix (a-d positional-list adt) plist:))
  (begin
    (define pq-item-make cons)
    (define pq-item-val car)
    (define pq-item-priority cdr)
    (define (lift func)
      (lambda (item1 item2)
        (func (pq-item-priority item1)
              (pq-item-priority item2))))
 
    (define-record-type priority-queue
      (make l g)
      priority-queue?
      (l plist)
      (g greater))
 
    (define (new >>?)
      (make (plist:new eq?) (lift >>?)))
 
    (define (full? pq)
      (plist:full? (plist pq)))
 
    (define (empty? pq)
      (plist:empty? (plist pq)))
 
    (define (enqueue! pq val pty)
      (plist:add-before! (plist pq) (pq-item-make val pty))
      pq)
 
    (define (serve! pq)
      (define plst (plist pq))
      (define >>? (greater pq))
      (if (empty? pq)
          (error "priority queue empty (serve!)" pq))
      (let*
          ((highest-priority-position
            (let loop
              ((current-pos (plist:first plst))
               (maximum-pos (plist:first plst)))
              (if (plist:has-next? plst current-pos)
                  (loop (plist:next plst current-pos)
                        (if (>>? (plist:peek plst current-pos)
                                 (plist:peek plst maximum-pos))
                            current-pos
                            maximum-pos))
                  (if (>>? (plist:peek plst current-pos)
                           (plist:peek plst maximum-pos))
                      current-pos
                      maximum-pos))))
           (served-item (plist:peek plst highest-priority-position)))
        (plist:delete! plst highest-priority-position)
        (pq-item-val served-item)))
 
    (define (peek pq)
      (define plst (plist pq))
      (define >>? (greater pq))
      (if (empty? pq)
          (error "empty priority queue (peek)" pq))
      (let*
          ((highest-priority-position
            (let loop
              ((current-pos (plist:first plst))
               (minimum-pos (plist:first plst)))
              (if (plist:has-next? plst current-pos)
                  (loop (plist:next plst current-pos)
                        (if (>>? (plist:peek plst current-pos)
                                 (plist:peek plst minimum-pos))
                            current-pos
                            minimum-pos))
                  (if (>>? (plist:peek plst current-pos)
                           (plist:peek plst minimum-pos))
                      current-pos
                      minimum-pos))))
           (served-item (plist:peek plst highest-priority-position)))
        (pq-item-val served-item)))))