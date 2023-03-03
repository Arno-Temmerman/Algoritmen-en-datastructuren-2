#lang r5rs
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*               Complex Numbers (Object-Oriented Style)           *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2018 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universitent Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(#%provide make-complex)
    
(define (make-complex r i)
  (define (complex+ c)
    (make-complex (+ r (c 'real))
                  (+ i (c 'imag))))
  (define (complex* c)
    (make-complex (- (* r (c 'real))
                     (* i (c 'imag)))
                  (+ (* r (c 'imag))
                     (* i (c 'real)))))
  (define (complex- c)
    (make-complex (- r (c 'real))
                  (- i (c 'imag))))
  (define (complex/ c)
    (define denom (+ (* (c 'real)
                        (c 'real))
                     (* (c 'imag)
                        (c 'imag))))
    (define real (+ (* r (c 'real)) (* i (c 'imag))))
    (define imag (- (* i (c 'real)) (* r (c 'imag))))
    (make-complex (/ real denom) (/ imag denom)))
  (define (modulus)
    (sqrt (+ (* r r) (* i i))))
  (define (argument)
    (atan i r))
  (define (real)
    r)
  (define (imag)
    i)
  (lambda (message . args)
    (cond ((eq? message '+) (apply complex+ args))
          ((eq? message '-) (apply complex- args))
          ((eq? message '*) (apply complex* args))
          ((eq? message '/) (apply complex/ args))
          ((eq? message 'modulus) (modulus))
          ((eq? message 'argument) (argument))
          ((eq? message 'real) (real))
          ((eq? message 'imag) (imag))
          ((eq? message 'complex->list) (list 'complex r i))
          (else (display "Complex Number Message Not Understood")))))