#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Relational Table Schema                    *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-`
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


(define-library (schema)
  (export new open delete!
          nr-of-occupancy-bytes nr-of-attributes record-size type size capacity
          fixed-header-size disk position)
  (import (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:)
          (a-d file constants)
          (scheme base))
  (begin
    (define schema-nr-size 2)   ; numbers of the schema as in (natural 7) encoded with 2 bytes
    (define fixed-header-size (* 2 disk:block-ptr-size))    ; next, previous; variable # of occupancy bits to be added dynamically
 
    (define-record-type schema
      (make b)
      schema?
      (b block))

    (define cap-offset         0)
    (define rsz-offset         (+ cap-offset schema-nr-size))
    (define schema-size-offset (+ rsz-offset schema-nr-size))
    (define schema-info-offset (+ schema-size-offset 1)) ; size is only one byte
 
    (define (capacity! scma nmbr)
      (define blck (block scma))
      (disk:encode-fixed-natural! blck cap-offset schema-nr-size nmbr))
    (define (capacity scma)
      (define blck (block scma))
      (disk:decode-fixed-natural blck cap-offset schema-nr-size))
 
    (define (record-size! scma nmbr)
      (define blck (block scma))
      (disk:encode-fixed-natural! blck rsz-offset schema-nr-size nmbr))
    (define (record-size scma)
      (define blck (block scma))
      (disk:decode-fixed-natural blck rsz-offset schema-nr-size))
 
    (define (nr-of-attributes scma)
      (define blck (block scma))
      (disk:decode-byte blck schema-size-offset))
    (define (nr-of-attributes! scma nmbr)
      (define blck (block scma))
      (disk:encode-byte! blck schema-size-offset nmbr))
 
    (define (type! scma indx type)
      (define blck (block scma))
      (disk:encode-byte! blck (+ schema-info-offset indx)  type))
    (define (type scma indx)
      (define blck (block scma))
      (disk:decode-byte blck (+ schema-info-offset indx)))
 
    (define (size! scma indx nmbr)
      (define blck (block scma))
      (disk:encode-byte! blck (- disk:block-size 1 indx)  nmbr))
    (define (size scma indx)
      (define blck (block scma))
      (disk:decode-byte blck (- disk:block-size 1 indx)))
 
    (define (field-size fild)
      (cond ((eq? (car fild) 'natural) 
             (cadr fild))
            ((eq? (car fild) 'integer)
             (cadr fild))
            ((eq? (car fild) 'decimal)
             disk:real64)
            ((eq? (car fild) 'string)
             (cadr fild))
            (else (error "unsported field type" (car fild)))))
 
    (define (field-type fild)
      (cond ((eq? (car fild) 'natural) 
             natural-tag)
            ((eq? (car fild) 'integer)
             integer-tag)
            ((eq? (car fild) 'decimal)
             decimal-tag)
            ((eq? (car fild) 'string)
             string-tag)
            (else (error "unsported field type" (car fild)))))

    (define (types/sizes! scma atts)
      (let loop
        ((indx 0)
         (atts atts))
        (type! scma indx (field-type (car atts)))
        (size! scma indx (field-size (car atts)))
        (if (not (null? (cdr atts)))
            (loop (+ indx 1) (cdr atts)))))

    (define (sum-of-sizes atts)
      (let loop 
        ((atts atts)
         (sizs 0))
        (if (null? atts)
            sizs
            (loop (cdr atts) (+ (field-size (car atts)) sizs)))))
 
    (define (nr-of-occupancy-bytes schema)
      (define bits (capacity schema))
      (exact (ceiling (/ bits 8))))
 
    (define (disk scma)
      (disk:disk (block scma)))
 
    (define (position scma)
      (disk:position (block scma)))
 
    (define (new dsk atts)
      (define rsz (sum-of-sizes atts))
      (define cap (quotient (* (- disk:block-size fixed-header-size) 8) 
                            (+ (* rsz 8) 1)))
      (if (< cap 1)
          (error "tuples must fit in a block (new schema)" rsz))
      (let* ((blck (fs:new-block dsk))
             (scma (make blck)))
        (capacity!         scma cap)
        (record-size!      scma rsz)
        (nr-of-attributes! scma (length atts))
        (types/sizes!      scma atts)
        (disk:write-block! blck)
        scma))
 
    (define (open dsk bptr)
      (define blck (disk:read-block dsk bptr))
      (define scma (make blck))
      scma)

    (define (delete! scma)
      (fs:delete-block (block scma))) ))
