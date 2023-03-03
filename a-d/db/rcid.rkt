#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Record Identification Pointers                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (record-identifier)
  (export new bptr slot rcid->fixed fixed->rcid size null null?)
  (import (except (scheme base) null?)
          (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:))
  (begin
 
    (define (new bptr slot)
      (cons bptr slot))
 
    (define bptr car)
 
    (define slot cdr)
 
    (define (rcid->fixed rcid)
      (+ (* (expt 256 disk:block-idx-size) (bptr rcid)) (slot rcid)))

    (define (fixed->rcid num)
      (define radx (expt 256 disk:block-idx-size))
      (new (quotient num radx) (modulo num radx)))
 
    (define size (+ disk:block-ptr-size disk:block-idx-size))
 
    (define null (new fs:null-block 0))
 
    (define (null? rcid)
      (and (fs:null-block? (bptr rcid))
           (= (slot rcid) 0)))))
