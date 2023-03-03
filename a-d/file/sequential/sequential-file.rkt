#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                  Sequential Files (Shared Code)                 *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (sequential-file)
 (export make name delete! disk sequential-file? first first!
         header header! current current! buffer buffer!
         eof-tag eob-tag integer-tag decimal-tag natural-tag string-tag)
 (import (scheme base)
         (prefix (a-d disk config) disk:)
         (prefix (a-d disk file-system) fs:)
         (a-d file constants))
  (begin

 (define eof-tag 255) ; end of file
 (define eob-tag 254) ; end of block; but more stuff in the next block

 (define frst-offs 0)
 (define curr-offs disk:block-ptr-size)
 
 (define (first hder)
   (disk:decode-fixed-natural  hder frst-offs disk:block-ptr-size))
 (define (first! hder bptr)
   (disk:encode-fixed-natural! hder frst-offs disk:block-ptr-size bptr))
 (define (current hder)
   (disk:decode-fixed-natural  hder curr-offs disk:block-idx-size))
 (define (current! hder offs)
   (disk:encode-fixed-natural! hder curr-offs disk:block-idx-size offs))  

 (define-record-type sequential-file
   (make d n h b)
   sequential-file?
   (d disk)
   (n name)
   (h header header!)
   (b buffer buffer!))
 
 (define (delete! file)
   (define fnam (name file))
   (define hder (header file))
   (define fdsk (disk file))
   (fs:delete-chain! fdsk (first hder))
   (fs:delete-block hder)  ; delete header
   (fs:rm fdsk fnam))))    ; remove from directory