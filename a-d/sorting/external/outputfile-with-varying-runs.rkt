#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Outputfile with Varying Runs                 *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (outputfile-with-varying-runs)
  (export new open-write! reread! close-write! write! new-run! run-full? delete!)
  (import (scheme base)
          (a-d sorting external file-with-varying-runs)
          (prefix (a-d file sequential output-file) out:))
  (begin
 
    (define (new disk name sent)
      (define outp (out:new disk name))
      (make outp sent))
  
    (define (open-write! fwrs)
      (out:open-write! (file fwrs))
      fwrs)

    (define (reread! fwrs)
      (out:reread! (file fwrs))
      fwrs)
 
    (define (close-write! fwrs)
      (out:close-write! (file fwrs))
      fwrs)
  
    (define (run-full? fwrs)
      #f)
 
    (define (new-run! fwrs)
      (define sent (sentinel fwrs))
      (out:write! (file fwrs) sent))
  
    (define (write! fwrs rcrd)
      (out:write! (file fwrs) rcrd)
      fwrs)))