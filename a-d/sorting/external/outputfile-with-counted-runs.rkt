#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Output File with Counted Runs                 *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (outputfile-with-counted-runs)
  (export new delete!
          reread! close-write! write!
          run-length new-run!)
  (import (scheme base)
          (a-d sorting external file-with-counted-runs)
          (prefix (a-d file sequential output-file) out:))
  (begin
 
    (define (new disk name runl)
      (define outp (out:new disk name))
      (make outp runl))
  
    (define (reread! fwrs runl)
      (out:reread! (file fwrs))
      (run-length! fwrs runl)
      (records-gone! fwrs 0))
 
    (define (close-write! fwrs)
      (out:close-write! (file fwrs))
      fwrs)
 
    (define (new-run! fwrs)
      (records-gone! fwrs 0))
 
    (define (write! fwrs rcrd)
      (define leng (run-length fwrs))
      (define gone (records-gone fwrs))
      (define outp (file fwrs))
      (if (>= (records-gone fwrs) (run-length fwrs))
          (error "run entirely full" (name fwrs) (cons gone leng)))
      (out:write! outp rcrd)
      (records-gone! fwrs (+ gone 1))
      fwrs)))