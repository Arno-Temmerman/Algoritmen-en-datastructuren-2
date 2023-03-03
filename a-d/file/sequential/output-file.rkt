#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Sequential Output Files                      *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (output-file)
  (export new sequential-file? delete! name disk
          header header! current current! buffer buffer!
          open-write! close-write! reread! write!)
  (import (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:)
          (a-d file sequential sequential-file)
          (scheme base))
  (begin
 
    (define (new disk name)
      (define hder (fs:new-block disk))
      (define bffr (fs:new-block disk))
      (define file (make disk name hder bffr))
      (fs:mk disk name (disk:position hder))
      (first!  hder (disk:position bffr))
      (fs:next-bptr! bffr fs:null-block)
      (current! hder disk:block-ptr-size)
      (disk:write-block! hder)
      file)
 
    (define (open-write! disk name)
      (define bptr (fs:whereis disk name))
      (define hder (disk:read-block disk bptr))
      (define fptr (first hder))
      (define bffr (disk:read-block disk fptr))
      (define file (make disk name hder bffr))
      (current! hder disk:block-ptr-size)
      file)
 
    (define (reread! file)
      (define fdsk (disk file))
      (define hder (header file))
      (define fptr (first hder))
      (close-write! file)
      (buffer!  file (disk:read-block fdsk fptr))
      (current! hder disk:block-ptr-size)
      file)
 
    (define (close-write! file)
      (define hder (header file))
      (define bffr (buffer file))
      (define fdsk (disk file))
      (disk:write-block! hder)
      (if (> (block-bytes-free file) 0)
          (disk:encode-byte! bffr (current hder) eof-tag))
      (let ((rest-list (fs:next-bptr bffr)))
        (fs:next-bptr! bffr fs:null-block)
        (fs:delete-chain! fdsk rest-list))
      (disk:write-block! bffr))
 
    (define (block-bytes-free file)
      (define hder (header file))
      (define curr (current hder))
      (- disk:block-size curr))
 
    (define (provide-next-block! file)
      (define fdsk (disk file))
      (define hder (header file))
      (define bffr (buffer file))
      (define next (cond ((fs:null-block? (fs:next-bptr bffr))
                          (let ((newb (fs:new-block fdsk)))
                            (fs:next-bptr! newb fs:null-block)
                            (fs:next-bptr! bffr (disk:position newb))
                            (disk:write-block! bffr)
                            newb))
                         (else
                          (disk:write-block! bffr)
                          (disk:read-block fdsk (fs:next-bptr bffr)))))
      (buffer!  file next)
      (current! hder disk:block-ptr-size)) ; skip "next" pointer
 
    (define (claim-bytes! file nmbr)
      (define bffr (buffer file))
      (define hder (header file))
      (define curr (current hder))
      (when (< (block-bytes-free file) nmbr)
        (if (not (= curr disk:block-size))
            (disk:encode-byte! bffr curr eob-tag)) ;  does not fit in current block -> skip rest
        (provide-next-block! file)))
 
    (define (write-type-tag file ttag)
      (claim-bytes! file 1)
      (let* ((bffr (buffer file))
             (hder (header file))
             (curr (current hder)))
        (disk:encode-byte! bffr curr ttag)
        (current! hder (+ curr 1))))
 
    (define (write-natural file nmbr)
      (claim-bytes! file (+ (disk:natural-bytes nmbr) 1)) ; size byte
      (let* ((hder (header file))
             (curr (current hder))
             (bffr (buffer file)))
        (disk:encode-byte! bffr curr (disk:natural-bytes nmbr))
        (disk:encode-fixed-natural! bffr (+ curr 1) (disk:natural-bytes nmbr) nmbr)
        (current! hder (+ curr 1 (disk:natural-bytes nmbr)))))

    (define (write-integer file nmbr)
      (claim-bytes! file (+ (disk:integer-bytes nmbr) 1))
      (let* ((hder (header file))
             (curr (current hder))
             (bffr (buffer file)))
        (current! hder (+ curr (disk:encode-arbitrary-integer! bffr curr nmbr)))))
 
    (define (write-real file nmbr)
      (claim-bytes! file disk:real64)
      (let* ((hder (header file))
             (curr (current hder))
             (bffr (buffer file)))
        (current! hder (+ curr (disk:encode-real! bffr curr disk:real64 nmbr)))))
 
    (define (rollout-bytes file byts indx)
      (when (< indx (bytevector-length byts))
        (claim-bytes! file 1) ; we should be able to drop at least one character in the current buffer
        (let* ((hder (header file))
               (bffr (buffer file))
               (curr (current hder))
               (free (block-bytes-free file)))
          (cond ((< (- (bytevector-length byts) indx) free)
                 (disk:encode-bytes! bffr byts curr indx (- (bytevector-length byts) indx) )
                 (current! hder (+ curr (- (bytevector-length byts) indx))))
                (else
                 (disk:encode-bytes! bffr byts curr indx free)
                 (current! hder disk:block-size)
                 (rollout-bytes file byts (+ indx free)))))))
 
    (define (write-string file strg)
      (claim-bytes! file 1) ; start by writing the length of the string
      (let* ((hder (header file))
             (curr (current hder))
             (bffr (buffer file))
             (byts (string->utf8 strg)))
        (disk:encode-byte! bffr curr (bytevector-length byts))
        (current! hder (+ curr 1))
        (rollout-bytes file byts 0)))
 
    (define (natural? x)
      (and (integer? x) 
           (>= x 0)))
 
    (define (write! file sval)
      (cond ((natural? sval)
             (write-type-tag file natural-tag)
             (write-natural file (exact sval))) ; (natural 2.0) = #t
            ((integer? sval)
             (write-type-tag file integer-tag)
             (write-integer  file (exact sval))) ; (integer? 2.0) = #t
            ((real? sval)
             (write-type-tag file decimal-tag)
             (write-real     file sval))
            ((string? sval)
             (write-type-tag file string-tag)
             (write-string   file sval))
            (error "unsupported type (write!)" sval)))))