#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                               Paths                             *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2012 Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (path)
  (export new empty? pop! push! node slot clear!)
  (import (scheme base)
          (prefix (a-d stack linked) stck:))
  (begin
 
    (define new    stck:new)

    (define empty? stck:empty?)
 
    (define pop!  stck:pop!)

    (define (push! stck node slot)
      (stck:push! stck (cons node slot)))
 
    (define (node stck) 
      (car (stck:top stck)))
 
    (define (slot stck)
      (cdr (stck:top stck)))
 
    (define (clear! stck)
      (let loop
        ()
        (when (not (empty? stck))
          (pop! stck)
          (loop))))))