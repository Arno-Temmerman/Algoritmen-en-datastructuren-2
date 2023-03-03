#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*           Dictionary (Binary Search Tree Implementation)        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dictionary)
  (export new dictionary? insert! delete! find empty? full?)
  (import (scheme base)
          (prefix (a-d tree binary-search-tree) bst:))
  (begin
  
    (define make-assoc cons)
    (define assoc-key car)
    (define assoc-value cdr)

    (define (lift proc)
      (lambda (assoc1 assoc2)
        (proc (assoc-key assoc1)
              (assoc-key assoc2))))
 
    (define (new ==? <<?)
      (bst:new 
       (lift ==?)
       (lift <<?)))
 
    (define (dictionary? any)
      (bst:bst? any))
 
    (define (insert! dct key val)
      (bst:insert! dct (make-assoc key val))
      dct)
 
    (define (delete! dct key)
      (bst:delete! dct (make-assoc  key 'ignored))
      dct)
 
    (define (find dct key)
      (define assoc (bst:find dct (make-assoc key 'ignored)))
      (if assoc
          (assoc-value assoc)
          assoc))
 
    (define (empty? dct)
      (bst:empty? dct))
 
    (define (full? dct)
      (bst:full? dct))))