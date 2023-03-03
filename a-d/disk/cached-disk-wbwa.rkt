#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*               Cached Disk  (Write-back write-alloc)             *-*-
;-*-*                                                                 *-*-
;-*-*                 Niels Joncheere - Youri Coppens                 *-*-
;-*-*                2022 Department of Informatics (DINF)            *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; Originally created by dr. Niels Joncheere in order to prevent the locking of blocks
; The goal is to obtain a clean implementation of a write-back write-allocate cache
; Cf. https://en.wikipedia.org/wiki/Cache_(computing)#Writing_policies
; Cf. https://en.wikipedia.org/wiki/Cache_(computing)#/media/File:Write-back_with_write-allocation.svg

(define-library (cached-disk)
  (export block-size disk-size
          block-ptr-size block-idx-size real32 real64
          block? disk position 
          new mount unmount name disk-size
          read-block write-block!
          decode-byte encode-byte! 
          decode-fixed-natural encode-fixed-natural!
          decode-arbitrary-integer encode-arbitrary-integer! 
          integer-bytes natural-bytes
          decode-real encode-real! 
          decode-string encode-string!
          decode-bytes encode-bytes!)
  (import (scheme base)
          (a-d scheme-tools)
          (prefix (a-d disk disk) disk:))
  (begin
 
    (define real32 disk:real32)
    (define real64 disk:real64)
 
    (define block-size disk:block-size)
    (define disk-size  disk:disk-size)
    (define block-ptr-size disk:block-ptr-size)
    (define block-idx-size disk:block-idx-size)
 
    (define cache-size 10)
 
    (define (cache:new)
      (make-vector cache-size '()))
 
    (define (cache:get cche indx)
      (vector-ref cche indx))
 
    (define (cache:put! cche indx blck)
      (vector-set! cche indx blck))
 
    (define (cache:find-free-index cche)
      (define oldest-time (current-time))
      (define oldest-indx -1)
      (define (traverse indx)
        (if (< indx cache-size)
            (let ((blck (cache:get cche indx)))
              (if (null? blck)
                  indx
                  (let ((stmp (time-stamp blck)))
                    (when (time<=? stmp oldest-time) ; AANGEPAST
                      (set! oldest-time stmp)
                      (set! oldest-indx indx))
                    (traverse (+ indx 1)))))
            oldest-indx)) ; AANGEPAST oldest-indx should never be -1
      (traverse 0))
 
    (define (cache:find-block cche bptr)
      (define (position-matches? blck)
        (and (not (null? blck))
             (= (position blck) bptr)))
      (let traverse
        ((indx 0)
         (blck (cache:get cche 0)))
        (cond ((position-matches? blck)
               blck)
              ((< (+ indx 1) cache-size)
               (traverse (+ indx 1) 
                         (cache:get cche (+ indx 1))))
              (else
               '()))))
 
    (define-record-type cdisk
      (make-cdisk v d)
      disk?
      (v disk-cache)
      (d real-disk))
 
    (define (new name)
      (make-cdisk (cache:new) (disk:new name)))
 
    (define (mount name)
      (make-cdisk (cache:new) (disk:mount name)))
 
    (define (name cdsk)
      (disk:name (real-disk cdsk)))
 
    (define (unmount cdsk) 
      (define vctr (disk-cache cdsk))
      (define (traverse indx)
        (if (< indx cache-size)
            (let ((blck (cache:get vctr indx)))
              (cond
                ((not (null? blck))
                 (if (dirty? blck)
                     (disk:write-block! (block blck)))))
              (traverse (+ indx 1)))))
      (traverse 0)
      (disk:unmount (real-disk cdsk)))
 
    (define (write-block! blck) ; write requested
      (define cche (disk-cache (disk blck)))
      (define cache-blck (cache:find-block cche (position blck))) ; TOEGEVOEGD

      ; HEEL DE IF-expressie is toegevoegd
      (if (null? cache-blck)                            ; cache-blck is null -> cache miss
          (let* ((indx (cache:find-free-index cche))    ; locate cache block to use
                 (existing-blck (cache:get cche indx)))
            (if (and (not (null? existing-blck))        ; if there is an existing block and it is dirty, write it back to disk
                     (dirty? existing-blck)) 
                (disk:write-block! (block existing-blck)))        
            ; (we don't need to read the block from disk, as the user has provided us with it)
            (cache:put! cche indx blck)) ; store the block in the cache
          ; cache-blck is not null -> cache hit, do nothing
          )
      (dirty! blck #t)                   ; AANGEPAST no write, that's the whole point!
      (time-stamp! blck (current-time))) ; TOEGEVOEGD
 
    (define (read-block cdsk bptr) ; read requested
      (define cche (disk-cache cdsk))
      (define blck (cache:find-block cche bptr))
      (if (null? blck) ; blck is null -> cache miss
          (let* ((indx (cache:find-free-index cche)) ; locate cache block to use
                 (existing-blck (cache:get cche indx)))
            (if (and (not (null? existing-blck))
                     (dirty? existing-blck))                     ; AANGEPAST: if there is an existing block and it is dirty, write it back to disk
                 (disk:write-block! (block existing-blck)))      ; AANGEPAST
            (set! blck (make-block cdsk (disk:read-block (real-disk cdsk) bptr))) ; read requested block from disk
            (cache:put! cche indx blck))                                          ; ... and store it in the cache
          ; blck is not null -> cache hit, do nothing
          )
      ; no more locking!
      (time-stamp! blck (current-time)) ; TOEGEVOEGD
      blck)
 
    (define-record-type cblock
      (make d t i b)
      block?
      (d dirty? dirty!)
      ; AANGEPAST no more locking!
      (t time-stamp time-stamp!)
      (i disk)
      (b block block!))
 
    (define (make-block cdsk blck)
      (make #f (current-time) cdsk blck)) ; AANGEPAST no more locking
 
    (define (position blck) 
      (disk:position (block blck))) ; AANGEPAST no more invalidation
 
    (define integer-bytes disk:integer-bytes)
 
    (define natural-bytes disk:natural-bytes)
 
    (define (make-cached-encoder proc) ; AANGEPAST no more checks on validness + not updating dirty-bit or time-stamp 
      (lambda args      
        (define blck (car args))
        (apply proc (cons (block blck) (cdr args)))))
 
    (define encode-byte!              (make-cached-encoder disk:encode-byte!))
    (define encode-fixed-natural!     (make-cached-encoder disk:encode-fixed-natural!))
    (define encode-arbitrary-integer! (make-cached-encoder disk:encode-arbitrary-integer!))
    (define encode-real!              (make-cached-encoder disk:encode-real!))
    (define encode-string!            (make-cached-encoder disk:encode-string!))
    (define encode-bytes!             (make-cached-encoder disk:encode-bytes!))
 
    (define (make-cached-decoder proc) ; AANGEPAST no more checks on validness + not updating dirty-bit or time-stamp 
      (lambda args
        (define blck (car args))
        (apply proc (cons (block blck) (cdr args)))))
 
    (define decode-byte              (make-cached-decoder disk:decode-byte))
    (define decode-fixed-natural     (make-cached-decoder disk:decode-fixed-natural))
    (define decode-arbitrary-integer (make-cached-decoder disk:decode-arbitrary-integer))
    (define decode-real              (make-cached-decoder disk:decode-real))
    (define decode-string            (make-cached-decoder disk:decode-string))
    (define decode-bytes             (make-cached-decoder disk:decode-bytes))))
