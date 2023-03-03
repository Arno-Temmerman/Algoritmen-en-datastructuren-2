#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*              Complex Numbers (Using `SRFI Records)              *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2011 Software Languages Lab                   *-*-
;-*-*                   Vrije Universitent Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library 
 (complex)
 (export new complex? real imag + - / * modulus argument)
 (import (srfi :9)
         (rename (except (rnrs base) complex?)
                 (+ number+) (* number*) (/ number/) (- number-)))
   
 (define-record-type complex
   (new r i)
   complex?
   (r real)
   (i imag))
   
 (define (+ c1 c2)
   (define result-real (number+ (real c1) (real c2)))
   (define result-imag (number+ (imag c1) (imag c2)))
   (new result-real result-imag))
 
 (define (* c1 c2)
   (define result-real (number- (number* (real c1) (real c2))
                                (number* (imag c1) (imag c2))))
   (define result-imag (number+ (number* (real c1) (imag c2))
                                (number* (imag c1) (real c2))))
   (new result-real result-imag))
 
 (define (- c1 c2)
   (define result-real (number- (real c1) (real c2)))
   (define result-imag (number- (imag c1) (imag c2)))
   (new result-real result-imag))
 
 (define (/ c1 c2)
   (define denom (number+ (number* (real c2)
                                   (real c2))
                          (number* (imag c2)
                                   (imag c2))))
   (define result-real (number+ (number* (real c1)
                                         (real c2))
                                (number* (imag c1)
                                         (imag c2))))
   (define result-imag (number- (number* (imag c1)
                                         (real c2))
                                (number* (real c1)
                                         (imag c2))))
   (new (number/ result-real denom) (number/ result-imag denom)))
 
 (define (modulus c)
   (sqrt (number+ (number* (real c) (real c)) 
                  (number* (imag c) (imag c)))))
 
 (define (argument c)
   (atan (imag c) (real c))))