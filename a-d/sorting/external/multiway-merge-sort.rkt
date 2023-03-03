#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Balanced Multiway Merge Sort                 *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (multiway-merge-sort)
  (export sort!)
  (import (scheme base)
          (rename (a-d sorting internal comparative quicksort-m3-bounded) (sort quicksort))
          (prefix (a-d heap standard) heap:)
          (prefix (a-d file sequential input-file) in:)
          (prefix (a-d file sequential output-file) out:)
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
 
    (define (write-run! ofcr imax)
      (let loop
        ((indx 0))
        (ofcr:write! ofcr (vector-ref irun indx))
        (if (< (+ indx 1) imax)
            (loop (+ indx 1)))))
 
    (define (make-aux-bundle disks)
      (define p (floor (/ (vector-length disks) 2)))
      (define in  (make-vector p))
      (define out (make-vector p))
      (define name "aux-")
      (do ((i 0 (+ i 1)))
        ((= i p))
        (vector-set! out i (ofcr:new (vector-ref disks i) 
                                     (string-append name (number->string i))
                                     rlen))
        (vector-set! in i (ofcr:new (vector-ref disks (+ p i))
                                    (string-append name (number->string (+ p i)))
                                    rlen))
        (ofcr:reread! (vector-ref in i) rlen)) ; we need input files in "in" (c.f. rewrite! in next phase)!
      (make-bundle p in out))
 
    (define (delete-aux-bundle! files)
      (for-each-input files
                      (lambda (file indx)
                        (ifcr:delete! file)))
      (for-each-output files
                       (lambda (file indx)
                         (ofcr:delete! file))))
 
    (define (make-bundle p in out)
      (cons p (cons in out)))
    (define (order files)
      (car files))
    (define (input files indx)
      (vector-ref (cadr files) indx))
    (define (output files indx)
      (vector-ref (cddr files) indx))
 
    (define (for-each-input files proc)
      (define nrfs (order files))
      (do ((indx 0 (+ indx 1)))
        ((= indx nrfs))
        (proc (input files indx) indx)))
    (define (for-each-output files proc)
      (define nrfs (order files))
      (do ((indx 0 (+ indx 1)))
        ((= indx nrfs))
        (proc (output files indx) indx)))
 
    (define (swap-files!? files)
      (define (switch-refs)
        (define tmp input)
        (set! input  output)
        (set! output tmp))
      (define p (order files))
      (define old-run-length (ofcr:run-length (output files 0)))
      (define new-run-length (* p old-run-length))
      (for-each-output files (lambda (outp indx)
                               (ofcr:reread! outp old-run-length)))
      (for-each-input files (lambda (inpt indx)
                              (ifcr:rewrite! inpt new-run-length)))
      (switch-refs)
      (ifcr:has-more? (input files 1)))
 
    (define (next-file indx p)
      (modulo (+ indx 1) p))
 
    (define (distribute! inpt files <<?)
      (define p (order files))
      (let loop
        ((indx 0))
        (let ((nmbr (read-run! inpt)))
          (when (not (= nmbr 0))
            (quicksort irun nmbr <<?)
            (write-run! (output files indx) nmbr)
            (ofcr:new-run! (output files indx))
            (loop (next-file indx p)))))
      (swap-files!? files))
  
    (define (collect! files inpt)
      (define last (input files 0))
      (in:rewrite! inpt)
      (let loop
        ((rcrd (ifcr:read last)))
        (out:write! inpt rcrd)
        (if (ifcr:run-has-more? last)
            (loop (ifcr:read last))))
      (out:close-write! inpt))

    (define (read-from-files? heap files)
      (for-each-input 
       files 
       (lambda (file indx)
         (when (ifcr:has-more? file)
           (ifcr:new-run! file)
           (heap:insert! heap (cons indx (ifcr:read file))))))
      (not (heap:empty? heap)))

    (define (serve heap files)
      (define el (heap:delete! heap))
      (define indx (car el))
      (define rcrd (cdr el))
      (if (ifcr:run-has-more? (input files indx))
          (heap:insert! heap (cons indx (ifcr:read (input files indx)))))
      rcrd)
 
    (define (merge! files <<?)
      (define heap (heap:new (order files)
                             (lambda (c1 c2) 
                               (<<? (cdr c1) (cdr c2)))))
      (let merge-files
        ((out-idx 0))
        (cond ((read-from-files? heap files)
               (let merge-p-runs
                 ((rcrd (serve heap files)))
                 (ofcr:write! (output files out-idx) rcrd)
                 (if (not (heap:empty? heap))
                     (merge-p-runs (serve heap files))))
               (ofcr:new-run! (output files out-idx))
               (merge-files (next-file out-idx (order files))))
              ((swap-files!? files)
               (merge-files 0)))))

    (define (sort! file dsks <<?)
      (define files (make-aux-bundle dsks))
      (distribute! file files <<?)
      (merge! files <<?)
      (collect! files file)
      (delete-aux-bundle! files))))