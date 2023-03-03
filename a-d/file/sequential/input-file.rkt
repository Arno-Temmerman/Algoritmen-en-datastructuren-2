#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Sequential Input Files                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (input-file)
  (export sequential-file? delete! name 
          ;header header! current-offset current-offset! buffer buffer!
          open-read! rewrite! close-read! read peek has-more?)
  (import (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:)
          (a-d file sequential sequential-file)
          (scheme base))
  (begin

    (define (open-read! disk name)
      (define bptr (fs:whereis disk name))
      (define hder (disk:read-block disk bptr))
      (define fptr (first hder))
      (define bffr (disk:read-block disk fptr))
      (define file (make disk name hder bffr))
      (current! hder disk:block-ptr-size) ; skip the "next" pointer
      file)
  
    (define (rewrite! file)
      (define fdsk (disk file))
      (define hder (header file))
      (define fptr (first hder))
      (close-read! file)
      (buffer!  file (disk:read-block fdsk fptr))
      (current! hder disk:block-ptr-size) ;  skip the "next" pointer
      file)
  
    (define (close-read! file)
      '()); nothing to write

    (define (more-on-buffer? file)
      (define bffr (buffer file))
      (define hder (header file))
      (define offs (current hder))
      (and (< offs disk:block-size)
           (not (or (= (disk:decode-byte bffr offs) eob-tag)
                    (= (disk:decode-byte bffr offs) eof-tag)))))
 
    (define (has-more? file)
      (define bffr (buffer file))
      (or (more-on-buffer? file)
          (not (fs:null-block? (fs:next-bptr bffr)))))
 
    (define (read-next-block! file)
      (define fdsk (disk file))
      (define hder (header file))
      (define bffr (buffer file))
      (define next-bptr (fs:next-bptr bffr))
      (define next-blck (disk:read-block fdsk next-bptr))
      (buffer!  file next-blck)
      (current! hder disk:block-ptr-size)) ; skip "next" pointer
 
    (define (supply-bytes! file)
      (define bffr (buffer file))
      (define hder (header file))
      (define curr (current hder))
      (if (not (more-on-buffer? file))
          (read-next-block! file)))
 
    (define (read-type-tag file)
      (supply-bytes! file)
      (let* ((hder (header file))
             (bffr (buffer file))
             (curr (current hder))
             (ttag (disk:decode-byte bffr curr)))
        (current! hder (+ curr 1))
        ttag))
 
    (define (peek-natural file)
      (supply-bytes! file)
      (let* ((hder (header file))
             (bffr (buffer file))
             (curr (current hder))
             (size (disk:decode-byte bffr curr)))
        (cons (disk:decode-fixed-natural bffr (+ curr 1) size) (+ 1 curr size))))
  
    (define (peek-integer file)
      (supply-bytes! file)
      (let* ((hder (header file))
             (bffr (buffer file))
             (curr (current hder)))
        (disk:decode-arbitrary-integer bffr curr)))

    (define (peek-real file) 
      (supply-bytes! file)
      (let* ((hder (header file))
             (bffr (buffer file))
             (curr (current hder)))
        (cons (disk:decode-real bffr curr disk:real64) (+ curr disk:real64))))
 
    (define (rollin-bytes file byts indx leng)
      (define hder (header file))
      (define bffr (buffer file))
      (define curr (current hder))
      (cond ((= (bytevector-length byts) 0) ; the empty string has length zero 
             curr)
            (else
             (supply-bytes! file)
             (set! curr (current hder))
             (set! bffr (buffer file))
             (cond ((< (+ curr leng) disk:block-size) 
                    (disk:decode-bytes bffr byts curr indx leng)
                    (+ curr leng))
                   (else
                    (current! hder disk:block-size)
                    (disk:decode-bytes bffr byts curr indx (- disk:block-size curr))
                    (rollin-bytes file byts 
                                  (+ indx (- disk:block-size curr)) 
                                  (- leng (- disk:block-size curr))))))))
 
    (define (peek-string file)
      (supply-bytes! file)
      (let* ((hder (header file))
             (bffr (buffer file))
             (curr (current hder))
             (leng (disk:decode-byte bffr curr)))
        (current! hder (+ curr 1))
        (let* ((byts (make-bytevector leng))
               (cur2 (rollin-bytes file byts 0 leng)))
          (cons (utf8->string byts) cur2))))
 
    (define (read file)
      (define hder (header file))
      (define curr (current hder))
      (let* ((ttag (read-type-tag file))
             (vcur (cond ((= ttag natural-tag)
                          (peek-natural file))
                         ((= ttag integer-tag)
                          (peek-integer file))
                         ((= ttag decimal-tag)
                          (peek-real file))
                         ((= ttag string-tag)
                          (peek-string file))
                         ((= ttag eof-tag)
                          (cons '() curr))
                         (else
                          (error "unsupported type on file (read)" ttag)))))
        (current! hder (cdr vcur))
        (car vcur)))
 
    (define (peek file)
      (define hder (header file))
      (let* ((bffr (buffer file))
             (curr-offs (current hder))
             (ttag (read-type-tag file))
             (res (car (cond ((= ttag natural-tag)
                              (peek-natural file))
                             ((= ttag integer-tag)
                              (peek-integer file))
                             ((= ttag decimal-tag)
                              (peek-real file))
                             ((= ttag string-tag)
                              (peek-string file))
                             ((= ttag eof-tag)
                              (cons '() curr-offs))
                             (else
                              (error "unsupported type on file (peek)" ttag))))))
        (current! hder curr-offs)
        (buffer! file bffr)
        res))))