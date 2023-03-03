#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Exception Handling Implementation                *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (try-catch)
  (export throw try-catch)       
  (import (scheme base))
  (begin 
    (define (*throw* exception)
      (error "No Exception Handler " exception))
   
    (define (throw exception) ; R6RS does not allow mutation of exported variables :-(
      (*throw* exception))
 
    (define (try-catch
             try-lambda
             filter
             handler)
      (call/cc
       (lambda (cont)
         (define keep *throw*)
         (set! *throw* (lambda (exception)
                         (set! *throw* keep)
                         (if (filter exception)
                             (cont (handler exception))
                             (throw exception))))
         (let ((result (try-lambda)))
           (set! *throw* keep)
           result))))))