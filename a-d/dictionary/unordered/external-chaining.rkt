#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Hash Tables (External Chaining)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library 
  (dictionary)
  (export new dictionary? insert! delete! find)
  (import (scheme base))
  (begin
    
    (define make-assoc cons)
    (define assoc-key car)
    (define assoc-value cdr)
 
    (define-record-type external-chaining
      (make s h e)
      dictionary?
      (s storage)
      (h hash-function)
      (e equality))
 
    (define (new ==? M h)
      (make (make-vector M '()) (lambda (k) (remainder (h k) M)) ==?))
 
    (define (insert! table key val)
      (define vector (storage table))
      (define h (hash-function table))
      (define ==? (equality table))
      (define home-address (h key))
      (define assoc (make-assoc key val))
      (let insert-in-bucket 
        ((prev '())
         (next! (lambda (ignore next) 
                  (vector-set! vector home-address next)))
         (next (vector-ref vector home-address)))
        (cond 
          ((null? next)
           (next! prev (cons assoc next)))
          ((==? (assoc-key (car next)) key)
           (set-car! next assoc))
          (else
           (insert-in-bucket next set-cdr! (cdr next)))))
      table)
 
    (define (find table key)
      (define vector (storage table))
      (define h (hash-function table))
      (define ==? (equality table))
      (define home-address (h key))
      (let find-in-bucket 
        ((next (vector-ref vector home-address)))
        (cond
          ((null? next)
           #f)
          ((==? (assoc-key (car next)) key)
           (assoc-value (car next)))
          (else
           (find-in-bucket (cdr next))))))
 
    (define (delete! table key)
      (define vector (storage table))
      (define h (hash-function table))
      (define ==? (equality table))
      (define home-address (h key))
      (let delete-from-bucket 
        ((prev '())
         (next! (lambda (ignore next) (vector-set! vector home-address next)))
         (next (vector-ref vector home-address)))
        (cond 
          ((null? next)
           #f)
          ((==? (assoc-key (car next)) key)
           (next! prev (cdr next))
           table)
          (else
           (delete-from-bucket next set-cdr! (cdr next)))))
      table)))