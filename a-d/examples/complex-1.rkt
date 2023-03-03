#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Complex Numbers (Procedural Style)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universitent Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (complex)
  (export new complex? real imag + - / * modulus argument)
  (import (scheme inexact)
          (rename (except (scheme base) complex?)
                  (+ number+) (* number*) (/ number/) (- number-)))
  (begin
    (define complex-tag 'complex)
    (define (get-real c)
      (cadr c))
    (define (get-imag c)
      (cadr (cdr c)))
 
    (define (new r i)
      (list complex-tag r i))
 
    (define (complex? any)
      (and (pair? any)
           (eq? (car any) complex-tag)))
 
    (define (real c)
      (get-real c))
 
    (define (imag c)
      (get-imag c))
 
    (define (+ c1 c2)
      (define real (number+ (get-real c1) (get-real c2)))
      (define imag (number+ (get-imag c1) (get-imag c2)))
      (new real imag))
 
    (define (* c1 c2)
      (define real (number- (number* (get-real c1) (get-real c2))
                            (number* (get-imag c1) (get-imag c2))))
      (define imag (number+ (number* (get-real c1) (get-imag c2))
                            (number* (get-imag c1) (get-real c2))))
      (new real imag))
 
    (define (- c1 c2)
      (define real (number- (get-real c1) (get-real c2)))
      (define imag (number- (get-imag c1) (get-imag c2)))
      (new real imag))
 
    (define (/ c1 c2)
      (define denom (number+ (number* (get-real c2)
                                      (get-real c2))
                             (number* (get-imag c2)
                                      (get-imag c2))))
      (define real (number+ (number* (get-real c1)
                                     (get-real c2))
                            (number* (get-imag c1)
                                     (get-imag c2))))
      (define imag (number- (number* (get-imag c1)
                                     (get-real c2))
                            (number* (get-real c1)
                                     (get-imag c2))))
      (new (number/ real denom) (number/ imag denom)))
 
    (define (modulus c)
      (sqrt (number+ (number* (get-real c) (get-real c)) 
                     (number* (get-imag c) (get-imag c)))))
 
    (define (argument c)
      (atan (get-imag c) (get-real c)))))