#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Disks (Configuration File)                    *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (disk)
 (export block? disk position 
         new mount unmount name 
         block-size disk-size
         block-idx-size block-ptr-size
         real32 real64
         integer-bytes natural-bytes
         read-block write-block!
         decode-byte encode-byte! 
         decode-fixed-natural encode-fixed-natural!
         decode-arbitrary-integer encode-arbitrary-integer! 
         decode-real encode-real! 
         decode-string encode-string!
         decode-bytes encode-bytes!)
 (import (a-d disk disk)
         ;(a-d disk cached-disk)
         ;(a-d disk cached-disk-wbwa)
         ;(Hoofdstuk14 Oplossingen cached-disk-info) ; zie WPO
         ;(Hoofdstuk14 Oplossingen cached-disk-wbwa-info) ; zie WPO
         )
 )