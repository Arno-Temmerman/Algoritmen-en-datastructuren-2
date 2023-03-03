#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Dictionary (Sorted List Implementation)            *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (dictionary)
  (export new dictionary? insert! delete! find empty? full?)
  (import (scheme base)
          (prefix (a-d sorted-list linked) slist:))
  (begin
   
    (define make-assoc cons)
    (define assoc-key car)
    (define assoc-value cdr)

    (define (lift proc)
      (lambda (assoc1 assoc2)
        (proc (assoc-key assoc1)
              (assoc-key assoc2))))
 
    (define (new ==? <<?)
      (slist:new 
       (lift <<?)
       (lift ==?)))
 
    (define (dictionary? any)
      (slist:sorted-list? any))
 
    (define (insert! dct key val)
      (slist:add! dct (make-assoc key val)))
 
    (define (delete! dct key)
      (slist:find! dct (make-assoc key 'ignored))
      (if (slist:has-current? dct)
          (slist:delete! dct)))
 
    (define (find dct key)
      (slist:find! dct (make-assoc key 'ignored))
      (if (slist:has-current? dct)
          (assoc-value (slist:peek dct))
          #f))
 
    (define (empty? dct)
      (slist:empty? dct))
 
    (define (full? dct)
      (slist:full? dct))))