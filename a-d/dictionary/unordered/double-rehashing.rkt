#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                 Hash Tables (Double Rehashing)                  *-*-
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
 
    (define-record-type double-rehashing
      (make s h1 h2 e)
      dictionary?
      (s  storage)
      (h1 hash-function1)
      (h2 hash-function2)
      (e  equality))
 
    (define (new ==? M h1 h2)
      (make (make-vector M 'empty)  (lambda (x) (remainder (h1 x) M)) h2 ==?))
 
    (define (rehash key address h2 M)
      (remainder (+ address (h2 key)) M))
 
    (define (insert! table key val)
      (define vector (storage table))
      (define h1 (hash-function1 table))
      (define h2 (hash-function2 table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h1 key)))
        (let ((assoc (vector-ref vector address)))
          (cond 
            ((or (eq? assoc 'empty)
                 (eq? assoc 'deleted))
             (vector-set! vector address (make-assoc key val)))
            ((==? (assoc-key assoc) key)
             (vector-set! vector address (make-assoc key val)))
            (else
             (rehash-iter (rehash 
                           key
                           address
                           h2
                           (vector-length vector)))))))
      table)
 
    (define (find table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h1 (hash-function1 table))
      (define h2 (hash-function2 table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h1 key)))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((eq? assoc 'deleted)
             (rehash-iter (rehash key address h2 M)))
            ((==? (assoc-key assoc) key)
             (assoc-value assoc))
            (else
             (rehash-iter (rehash key address h2 M)))))))
 
    (define (delete! table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h1 (hash-function1 table))
      (define h2 (hash-function2 table))
      (define ==? (equality table))
      (let rehash-iter
        ((address (h1 key)))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((eq? assoc 'deleted)
             (rehash-iter (rehash key address h2 M)))
            ((==? (assoc-key assoc) key)
             (vector-set! vector address 'deleted))
            (else
             (rehash-iter (rehash key address h2 M))))))
      table)))