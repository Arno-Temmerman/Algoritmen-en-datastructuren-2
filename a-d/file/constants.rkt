#lang r7rs

(define-library (constants)
  (export natural-tag integer-tag decimal-tag string-tag
          encoders decoders
          equals smaller greater sentinel-for
          done no-current next-higher not-found duplicate 
          utf8-sentinel-for)
  (import (scheme base)
          (prefix (a-d disk config) disk:))
  (begin
    (define natural-tag 0)
    (define integer-tag 1)
    (define decimal-tag 2)
    (define string-tag  3)
 
    (define encoders  (vector disk:encode-fixed-natural! 
                              disk:encode-arbitrary-integer!
                              disk:encode-real! 
                              disk:encode-string!))
    (define decoders  (vector disk:decode-fixed-natural
                              disk:decode-arbitrary-integer
                              disk:decode-real  
                              disk:decode-string))
    (define equals    (vector = = = string=?))
    (define smaller   (vector < < < string<?))
    (define greater   (vector > > > string>?))
 
    (define (sentinel-for ktyp ksiz)
      (cond ((= ktyp natural-tag)
             (- (expt 256 ksiz) 1))
            ((= ktyp integer-tag)
             (- (quotient (expt 256 ksiz) 2) 1))
            ((= ktyp decimal-tag)
             +inf.0)
            ((= ktyp string-tag)
             (utf8-sentinel-for ksiz))))
 
    (define done        'done)
    (define no-current  'no-current)
    (define next-higher 'next-higher)
    (define not-found   'not-found)
    (define duplicate   'duplicate)
 
    (define (utf8-sentinel-for nmbr-byts)
      (define byts (make-bytevector nmbr-byts))
      (define (fill! offset rem)
        (cond ((= rem 1) 
               (bytevector-u8-set! byts offset 127))
              ((= rem 2) 
               (bytevector-u8-set! byts offset 223)
               (bytevector-u8-set! byts (+ offset 1) 191))
              (else
               (bytevector-u8-set! byts offset 239)
               (bytevector-u8-set! byts (+ offset 1) 191)
               (bytevector-u8-set! byts (+ offset 2) 191)
               (if (> rem 3)
                   (fill! (+ offset 3) (- rem 3))))))
      (fill! 0 nmbr-byts)
      (utf8->string byts))))