#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     File System Abstraction                     *-*-
;-*-*                                                                 *-*-
;-*-*              Wolfgang De Meuter - Youri Coppens                 *-*-
;-*-*              2018 - 2022 Software Languages Lab                 *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (file-system)
  (export format! new-block delete-block delete-chain!
          write-meta-block! read-meta-block directory
          next-bptr next-bptr! null-block? null-block filename-size
          mk ls rm whereis df)
  (import (scheme base)
          (prefix (a-d disk config) disk:)
          (a-d file constants))
  (begin
 
    (define filename-size     10)
    (define sentinel-filename (utf8-sentinel-for filename-size))
 
    (define null-block 0)
    (define meta-bptr  null-block)

    ; organisation of the meta block
    (define directory-offset 0)                         ; byte nr of the directory pointer in the meta block
    (define freelist-offset  disk:block-ptr-size)       ; byte nr of the freelist pointer in the meta block
    (define available-offset (* 2 disk:block-ptr-size)) ; byte nr of the 'blocks available' count in the meta block
 
    (define (directory meta)
      (disk:decode-fixed-natural  meta directory-offset disk:block-ptr-size))
    (define (directory! meta blck)
      (disk:encode-fixed-natural! meta directory-offset disk:block-ptr-size blck))
    (define (freelist meta)
      (disk:decode-fixed-natural  meta freelist-offset  disk:block-ptr-size))
    (define (freelist! meta flst)
      (disk:encode-fixed-natural! meta freelist-offset  disk:block-ptr-size flst))
    (define (blocks-free meta)
      (disk:decode-fixed-natural  meta available-offset disk:block-ptr-size))
    (define (blocks-free! meta free)
      (disk:encode-fixed-natural! meta available-offset disk:block-ptr-size free))
 
    (define next-offset 0) 
    (define slot-size   (+ filename-size disk:block-ptr-size))

    (define (next-bptr blck)
      (disk:decode-fixed-natural blck next-offset disk:block-ptr-size))
    (define (next-bptr! blck bptr)
      (disk:encode-fixed-natural! blck next-offset disk:block-ptr-size bptr))
 
    (define (dir-name/bptr! blck slot name bptr)
      (define offn (+ disk:block-ptr-size (* slot slot-size)) )
      (define offp (+ disk:block-ptr-size (* slot slot-size) filename-size))
      (disk:encode-string! blck offn filename-size name)
      (disk:encode-fixed-natural! blck offp disk:block-ptr-size bptr))
    (define (dir-name blck slot)
      (define offn (+ disk:block-ptr-size (* slot slot-size)))
      (disk:decode-string blck offn filename-size))
    (define (dir-bptr blck slot)
      (define offp (+ disk:block-ptr-size (* slot slot-size) filename-size))
      (disk:decode-fixed-natural blck offp disk:block-ptr-size))   
 
    (define (null-block? bptr)
      (= bptr null-block))
 
    (define (has-next? blck)
      (not (null-block? (next-bptr blck))))
 
    (define (empty-slot? blck slot)
      (string=? sentinel-filename (dir-name blck slot)))

    (define nr-of-dir-slots (quotient (- disk:block-size disk:block-ptr-size) slot-size))
 
    (define (at-end? blck slot)
      (= slot nr-of-dir-slots))
 
    (define (read-meta-block disk)
      (disk:read-block disk meta-bptr))
 
    (define (write-meta-block! meta)
      (disk:write-block! meta))
 
    (define (format! disk)
      (define meta (read-meta-block disk))
      (directory!   meta null-block)
      (freelist!    meta (+ meta-bptr 1)) 
      (blocks-free! meta (- disk:disk-size 1)); the superblock itself is not free
      (write-meta-block! meta)
      (let high-level-format
        ((bptr (+ meta-bptr 1)))
        (let ((block (disk:read-block disk bptr)))
          (cond ((< (+ bptr 1) disk:disk-size)
                 (next-bptr! block (+ bptr 1))
                 (disk:write-block! block)
                 (high-level-format (+ bptr 1)))
                (else
                 (next-bptr! block null-block) ; end of freelist
                 (disk:write-block! block)))))
      disk)
 
    (define (df disk)
      (define meta (read-meta-block disk))
      (blocks-free meta))
 
    (define (new-block disk)
      (define meta (read-meta-block disk))
      (define flst (freelist meta))
      (if (null-block? flst)
          (error "disk full! (new-block)" disk)
          (let* ((blck (disk:read-block disk flst)))
            (blocks-free! meta (- (blocks-free meta) 1))
            (freelist!    meta (next-bptr blck))
            (write-meta-block! meta)
            blck)))
 
    (define (delete-block blck)
      (define disk (disk:disk blck))
      (define meta (read-meta-block disk))
      (next-bptr! blck (freelist meta))
      (disk:write-block!  blck)
      (freelist!    meta (disk:position blck))
      (blocks-free! meta (+ (blocks-free meta) 1))
      (write-meta-block! meta))
 
    (define (delete-chain! disk bptr)
      (unless (null-block? bptr)
        (let* ((blck (disk:read-block disk bptr))
               (next (next-bptr blck)))
          (delete-block blck)
          (delete-chain! disk next))))
 
    (define (cap-name name)
      (if (> (string-length name) filename-size)
          (substring name 0 filename-size)
          name)) ; truncate string to max file name length
 
    (define (maybe-delete-block! blck slot next!)
      (cond 
        ((at-end? blck slot)      ; block appears to be completely empty
         (next! (next-bptr blck)) ;   => delete it
         (delete-block blck))
        ((empty-slot? blck slot)  ; continue checking
         (maybe-delete-block! blck (+ slot 1) next!))))
 
    (define (fresh-block! disk next!)
      (define next (new-block disk))
      (next-bptr! next null-block)
      (do ((slot 0 (+ slot 1)))
        ((at-end? next slot)
         (next! next)
         next)
        (dir-name/bptr! next slot sentinel-filename null-block)))
  
    (define (mk disk name bptr)
      (define meta (read-meta-block disk))
      (let loop-dir
        ((dptr (directory meta))
         (new! (lambda (newb)
                 (let ((meta (read-meta-block disk)))
                   (directory! meta (disk:position newb))
                   (write-meta-block! meta)))))
        (let ((blck (if (null-block? dptr)
                        (fresh-block! disk new!)
                        (disk:read-block disk dptr))))
          (let loop-block
            ((slot 0))
            (cond ((at-end? blck slot)
                   (loop-dir (next-bptr blck)
                             (lambda (newb) 
                               (next-bptr! blck (disk:position newb))
                               (disk:write-block! blck))))
                  ((empty-slot? blck slot)
                   (dir-name/bptr! blck slot name bptr)
                   (disk:write-block! blck))
                  (else
                   (loop-block (+ slot 1))))))))
 
    (define (rm disk name)
      (define meta (read-meta-block disk))
      (set! name (cap-name name))
      (let loop-dir
        ((bptr (directory meta))
         (nxt! (lambda (next) 
                 (directory! meta next)
                 (write-meta-block! meta))))
        (let ((blck (if (null-block? bptr)
                        (error "file not found (rm)" name)
                        (disk:read-block disk bptr))))
          (let loop-block
            ((slot 0)
             (seen #f))
            (cond ((at-end? blck slot)
                   (loop-dir (next-bptr blck) (lambda (next)
                                                (next-bptr! blck next)
                                                (disk:write-block! blck))))
                  ((empty-slot? blck slot)
                   (loop-block (+ slot 1) seen))
                  ((string=? name (dir-name blck slot))
                   (dir-name/bptr! blck slot sentinel-filename null-block)
                   (disk:write-block! blck)
                   (if (not seen)
                       (maybe-delete-block! blck slot nxt!)))
                  (else
                   (loop-block (+ slot 1) #t)))))))
  
    (define (ls disk)
      (define meta (read-meta-block disk))
      (define bptr (directory meta))
      (if (null-block? bptr)
          '()
          (let traverse-dir
            ((blck (disk:read-block disk bptr))
             (slot 0))
            (cond ((at-end? blck slot)
                   (if (has-next? blck)
                       (traverse-dir (disk:read-block disk (next-bptr blck)) 0)
                       '()))
                  ((empty-slot? blck slot)
                   (traverse-dir blck (+ slot 1)))
                  (else
                   (cons (cons (dir-name blck slot) (dir-bptr blck slot))
                         (traverse-dir blck (+ slot 1))))))))
 
    (define (whereis disk name)
      (define meta (read-meta-block disk))
      (define bptr (directory meta))
      (set!   name (cap-name name))
      (if (null-block? bptr)
          0
          (let traverse-dir 
            ((blck (disk:read-block disk bptr))
             (slot 0))
            (cond ((at-end? blck slot)
                   (if (has-next? blck)
                       (traverse-dir (disk:read-block disk (next-bptr blck)) 0)
                       null-block))
                  ((string=? name (dir-name blck slot))
                   (dir-bptr blck slot))
                  (else
                   (traverse-dir blck (+ slot 1)))))))))