#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Inputfile with Varying Runs                  *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (inputfile-with-varying runs)
  (export open-read rewrite! file close-read! new-run! delete!
          read peek has-more? run-has-more?)
  (import (scheme base)
          (a-d sorting external file-with-varying-runs)
          (prefix (a-d file sequential input-file) in:))
  (begin
  
    (define (open-read fwrs)
      (in:open-read! (file fwrs))
      fwrs)
 
    (define (rewrite! fwrs)
      (in:rewrite! (file fwrs))
      fwrs)
 
    (define (close-read! fwrs)
      (in:close-read! (file fwrs))
      fwrs)
  
    (define (run-has-more? fwrs)
      (and (has-more? fwrs)
           (not (eqv? (in:peek (file fwrs)) (sentinel fwrs)))))

    (define (has-more? fwrs)
      (in:has-more? (file fwrs)))
 
    (define (new-run! fwrs)
      (define rcrd (in:peek (file fwrs)))
      (if (eqv? rcrd (sentinel fwrs))
          (in:read (file fwrs)); eat the run marker
          (error "run not entirely consumed" (name fwrs) (sentinel fwrs))))
 
    (define (read fwrs)
      (define rcrd (in:peek (file fwrs)))
      (if (eqv? rcrd (sentinel fwrs))
          (error "run entirely consumed" (name fwrs) (sentinel fwrs))
          (in:read (file fwrs))))
 
    (define (peek fwrs)
      (in:peek (file fwrs)))))