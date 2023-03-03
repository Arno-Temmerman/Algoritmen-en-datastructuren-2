#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Hash Tables (Linear Rehashing)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dictionary)
  (export new dictionary? insert! delete! find)
  (import (scheme base))
  (begin
    
    (define make-assoc cons)
    (define assoc-key car)
    (define assoc-value cdr)
 
    (define-record-type linear-rehashing
      (make s h e)
      dictionary?
      (s storage)
      (h hash-function)
      (e equality))
 
    (define (new ==? M h)
      (make (make-vector M 'empty) (lambda (x) (remainder (h x) M)) ==?))
 
    (define c 1)
    (define (rehash address M)
      (remainder (+ address c) M))
 
    (define (insert! table key val)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (define new-assoc (make-assoc key val))
      (let rehash-iter 
        ((address (h key)))
        (let ((assoc (vector-ref vector address)))
          (cond ((or (eq? assoc 'empty)
                     (eq? assoc 'deleted))
                 (vector-set! vector address new-assoc))
                ((==? (assoc-key assoc) key)
                 (vector-set! vector address new-assoc))
                (else
                 (rehash-iter (rehash address M))))))
      table)
 
    (define (find table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h key)))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((eq? assoc 'deleted)
             (rehash-iter (rehash address M)))
            ((==? (assoc-key assoc) key)
             (assoc-value assoc))
            (else
             (rehash-iter (rehash address M)))))))
 
    (define (delete! table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h key)))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((eq? assoc 'deleted)
             (rehash-iter (rehash address M)))
            ((==? (assoc-key assoc) key)
             (vector-set! vector address 'deleted))
            (else
             (rehash-iter (rehash address M))))))
      table)))