#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Inputfile with Counted Runs                   *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (inputfile-with-counted-runs)
  (export rewrite! file close-read! delete!
          read peek
          run-length has-more? run-has-more? new-run!)
  (import (scheme base)
          (a-d sorting external file-with-counted-runs)
          (prefix (a-d file sequential input-file) in:))
  (begin
  
    (define (rewrite! fwrs runl)
      (in:rewrite! (file fwrs))
      (run-length! fwrs runl)
      (records-gone! fwrs 0))
 
    (define (close-read! fwrs)
      (in:close-read! (file fwrs))
      fwrs)
  
    (define (run-has-more? fwrs)
      (and (< (records-gone fwrs) 
              (run-length fwrs))
           (in:has-more? (file fwrs))))

    (define (has-more? fwrs)
      (in:has-more? (file fwrs)))
 
    (define (new-run! fwrs)
      (records-gone! fwrs 0))
  
    (define (read fwrs)
      (define gone (records-gone fwrs))
      (if (or (= gone (run-length fwrs)) (not (has-more? fwrs)))
          (error "run entirely consumed" (name fwrs) (run-length fwrs))
          (let ((rcrd (in:read (file fwrs))))
            (records-gone! fwrs (+ gone 1))
            rcrd)))
 
    (define (peek fwrs)
      (in:peek (file fwrs)))))