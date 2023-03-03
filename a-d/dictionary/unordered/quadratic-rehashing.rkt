#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*               Hash Tables (Quadratic Rehashing)                 *-*-
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
 
    (define-record-type quadratic-rehashing 
      (make s h e)
      dictionary?
      (s storage)
      (h hash-function)
      (e equality))
 
    (define (new ==? M h)
      (make (make-vector M 'empty) (lambda (x) (remainder (h x) M)) ==?))

    (define (rehash address j M)
      (remainder (+ address j) M))
 
    (define (insert! table key val)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (let rehash-iter 
        ((address (h key))
         (odd 1))
        (let ((assoc (vector-ref vector address)))
          (cond 
            ((or (eq? assoc 'empty)
                 (eq? assoc 'deleted))
             (vector-set! vector address (make-assoc key val)))
            ((==? (assoc-key assoc) key)
             (vector-set! vector address (make-assoc key val)))
            (else
             (rehash-iter (rehash address odd M) (+ odd 2))))))
      table)
 
    (define (find table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h key))
         (odd 1))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((eq? assoc 'deleted)
             (rehash-iter (rehash address odd M) (+ odd 2)))
            ((==? (assoc-key assoc) key)
             (assoc-value assoc))
            (else
             (rehash-iter (rehash address odd M) (+ odd 2)))))))
 
    (define (delete! table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h key))
         (odd 1))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((eq? assoc 'deleted)
             (rehash-iter (rehash address odd M) (+ odd 2)))
            ((==? (assoc-key assoc) key)
             (vector-set! vector address 'deleted))
            (else
             (rehash-iter (rehash address odd M) (+ odd 2))))))
      table)))