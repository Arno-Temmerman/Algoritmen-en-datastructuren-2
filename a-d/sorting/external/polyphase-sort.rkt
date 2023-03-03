#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                          Polyphase Sort                         *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (polyphase-sort)
  (export sort!)
  (import (scheme base)
          (rename (a-d sorting internal comparative quicksort-m3-bounded) (sort quicksort))
          (prefix (a-d heap standard) heap:)
          (prefix (a-d file sequential input-file) in:)
          (prefix (a-d file sequential output-file) out:)
          (prefix (a-d sorting external file-with-counted-runs) fwrs:)
          (prefix (a-d sorting external outputfile-with-counted-runs) ofcr:)
          (prefix (a-d sorting external inputfile-with-counted-runs) ifcr:))
  (begin
 
    (define rlen 10)
    (define irun (make-vector rlen))
 
    (define (read-run! file)
      (let loop
        ((indx 0))
        (cond ((or (= indx rlen) (not (in:has-more? file)))
               indx)
              (else
               (vector-set! irun indx (in:read file))
               (loop (+ indx 1))))))
  
    (define (padded-read-run! file sent)
      (define bsiz (read-run! file))
      (if (and (not (= bsiz rlen))
               (not (in:has-more? file)))
          (do ((pad bsiz (+ pad 1)))
            ((= pad rlen) bsiz)
            (vector-set! irun pad sent))
          bsiz))
 
    (define (write-run! ofcr imax)
      (let loop
        ((indx 0))
        (ofcr:write! ofcr (vector-ref irun indx))
        (if (< (+ indx 1) imax)
            (loop (+ indx 1)))))
  
    (define (make-aux-bundle disks)
      (define files (make-vector 3))
      (vector-set! files 0 (ofcr:new (vector-ref disks 0)
                                     "aux-0" rlen))
      (vector-set! files 1 (ofcr:new (vector-ref disks 1)
                                     "aux-1" rlen))
      (vector-set! files 2 (ofcr:new (vector-ref disks 2) 
                                     "aux-2" (+ rlen rlen)))
      files)
 
    (define (delete-aux-bundle! files)
      (fwrs:delete! (vector-ref files 0))
      (fwrs:delete! (vector-ref files 1))
      (fwrs:delete! (vector-ref files 2)))
 
    (define (output files)
      (vector-ref files 2))
 
    (define (input files i)
      (vector-ref files i))
  
    (define (next-phase!? files)
      (define irln (ifcr:run-length (input files 0)))
      (define orln (ofcr:run-length (output files)))
      (define last (vector-ref files 2))
      (vector-set! files 2 (vector-ref files 1))
      (vector-set! files 1 (vector-ref files 0))
      (vector-set! files 0 last)
      (ifcr:rewrite! (output files) (+ irln orln))
      (ofcr:reread! (input files 0) orln) 
      (ifcr:has-more? (input files 1)))

    (define (distribute! inpt files <<? sent)
      (define (swap-input files)
        (define temp (vector-ref files 0))
        (vector-set! files 0 (vector-ref files 1))
        (vector-set! files 1 temp))
      (let loop
        ((fib1 1)
         (fib2 0)
         (out-ctr 0)
         (nmbr (padded-read-run! inpt sent)))
        (cond ((< out-ctr fib1)
               (cond ((= nmbr 0) ; keep on writing dummy runs
                      (write-run! (input files 0) rlen)
                      (ofcr:new-run! (input files 0))
                      (loop fib1 fib2 (+ out-ctr 1) nmbr))
                     (else
                      (quicksort irun nmbr <<?)
                      (write-run! (input files 0) rlen)
                      (ofcr:new-run! (input files 0))
                      (loop fib1 fib2 (+ out-ctr 1) (padded-read-run!
                                                     inpt sent)))))
              ((in:has-more? inpt)
               (swap-input files)
               (loop (+ fib1 fib2) fib1 fib2 nmbr))))
      (ofcr:reread! (input files 0) (ifcr:run-length (input files 0)))
      (ofcr:reread! (input files 1) (ifcr:run-length (input files 1))))
  
    (define (collect! files inpt sent)
      (define last (input files 0))
      (in:rewrite! inpt)
      (let loop
        ((rcrd (ifcr:read last)))
        (out:write! inpt rcrd)
        (if (ifcr:run-has-more? last)
            (let ((rcrd (ifcr:read last)))
              (if (not (eq? rcrd sent))
                  (loop rcrd)))))
      (out:close-write! inpt))

    (define (read-from-files? heap files)
      (define ifcr1 (input files 0))
      (define ifcr2 (input files 1))
      (ifcr:new-run! (input files 1))
      (ifcr:new-run! (input files 0))
      (when (ifcr:has-more? ifcr2)
        (heap:insert! heap (cons 0 (ifcr:read ifcr1)))
        (heap:insert! heap (cons 1 (ifcr:read ifcr2))))
      (not (heap:empty? heap)))
 
    (define (serve heap files)
      (define el (heap:delete! heap))
      (define indx (car el))
      (define rcrd (cdr el))
      (if (ifcr:run-has-more? (input files indx))
          (heap:insert! heap (cons indx (ifcr:read (input files indx)))))
      rcrd)

    (define (merge! files <<?)
      (define heap (heap:new 2
                             (lambda (c1 c2)
                               (<<? (cdr c1) (cdr c2)))))
      (let merge-files 
        ()
        (cond ((read-from-files? heap files)
               (let merge-2-runs
                 ((rcrd (serve heap files)))
                 (ofcr:write! (output files) rcrd)
                 (if (not (heap:empty? heap))
                     (merge-2-runs (serve heap files))))
               (ofcr:new-run! (output files))
               (merge-files))
              ((next-phase!? files)
               (merge-files)))))

    (define (sort! file dsks <<? sent)
      (define files (make-aux-bundle dsks))
      (distribute! file files <<? sent)
      (merge! files <<?)
      (collect! files file sent)
      (delete-aux-bundle! files))))