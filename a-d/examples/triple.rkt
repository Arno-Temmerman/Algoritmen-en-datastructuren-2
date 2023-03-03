#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                            Triples                              *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2021 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


(define-library (triples)
  (export trons car cdr cmr set-car! set-cdr! set-cmr!)
  (import (scheme base))
  (begin
(define-record-type triple
  (trons a m d)
  triple?
  (a car set-car!)
  (m cmr set-cmr!)
  (d cdr set-cdr!))))

;(define t (trons 1 2 3))
;
;> (define t (trons 1 2 3))
;> (pair? t)
;#f
;> (triple? t)
;#t
;> (car t)
;1
;> (cdr t)
;3
;> (cmr t)
;2
;> (set-car! t "hi")
;> (set-cmr! t "di")
;> (set-cdr! t "ho")
;> t
;#<triple>
;> (car t)
;"hi"
;> (cmr t)
;"di"
;> (cdr t)
;"ho"