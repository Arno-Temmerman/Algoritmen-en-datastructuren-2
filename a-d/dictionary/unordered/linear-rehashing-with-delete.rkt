#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Hash Tables (Linear Rehashing + Delete)             *-*-
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
 
    (define (rehash index M)
      (remainder (+ index 1) M)) 
 
    (define (insert! table key value)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (define new-assoc (make-assoc key value))
      (let rehash-iter 
        ((address (h key)))
        (let ((assoc (vector-ref vector address)))
          (cond 
            ((eq? assoc 'empty)
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
            ((==? (assoc-key assoc) key)
             (assoc-value assoc))
            (else
             (rehash-iter (rehash address M)))))))
 
    (define (delete! table key)
      (define vector (storage table))
      (define M (vector-length vector))
      (define h (hash-function table))
      (define ==? (equality table))
      (define (between x <<1 y <<2 z)
        (and (<<1 x y)
             (<<2 y z)))
      (define (storage-move prev next)
        (if (eq? (vector-ref vector next) 'empty)
            prev
            (let ((home (h (assoc-key (vector-ref vector next)))))
              (if (or (between prev < home <= next)
                      (between home <= next < prev)
                      (between next < prev < home))
                  (storage-move prev (rehash next M))
                  (begin (vector-set! vector prev (vector-ref vector next))
                         (storage-move next (rehash next M)))))))
      (let rehash-iter 
        ((address (h key)))
        (let ((assoc (vector-ref vector address)))
          (cond
            ((eq? assoc 'empty)
             #f)
            ((==? (assoc-key assoc) key)
             (vector-set! 
              vector 
              (storage-move address (rehash address M)) 'empty))
            (else
             (rehash-iter (rehash address M))))))
      table)))