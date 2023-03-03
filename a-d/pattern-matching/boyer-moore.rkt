#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*            Pattern Matching (Boyer Moore Algorithm)             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library
 (boyer-moore)
 (export match)
 (import (rnrs base))
 
 (define (compute-bad-shift-table p)
   (define n-p (string-length p))
   (define min-ascii (char->integer (string-ref p 0)))
   (define max-ascii min-ascii)
   
   (define (calculate-table index)
     (if (< index n-p)
       (begin
         (set! min-ascii (min min-ascii (char->integer (string-ref p index))))
         (set! max-ascii (max max-ascii (char->integer (string-ref p index))))
         (calculate-table (+ index 1)))
       (make-vector (- max-ascii min-ascii -1) n-p)))
   
   (define (traverse index)
     (if (< (+ index 1) n-p)
       (let* ((ascii (char->integer (string-ref p index))))
         (vector-set! jump-table (- ascii min-ascii) (- n-p 1 index))
         (traverse (+ index 1)))))
   
   (define jump-table (calculate-table 0))
   (traverse 0)
   (lambda (c)
     (let ((ascii (char->integer c)))
       (if (>= max-ascii ascii min-ascii)
         (vector-ref jump-table (- ascii min-ascii))
         n-p))))
 
 (define (compute-good-shift-table p)
   (define n-p (string-length p))
   (define suffixes (make-vector (+ n-p 1) (+ n-p 1)))
   (define jump-table (make-vector (+ n-p 1) 0))
   (let loop
     ((j (+ n-p 1))
      (i n-p))
     (cond
       ((= i 0)
        (let loop 
          ((p (vector-ref suffixes 0))
           (j 0))
          (if (<= j n-p)
            (begin (if (= (vector-ref jump-table j) 0)
                     (vector-set! jump-table j p))
                   (loop (if (= p j)
                           (vector-ref suffixes p)
                           p)
                         (+ j 1))))))
       ((and (<= j n-p)
             (not (eq? (string-ref p (- i 1))
                       (string-ref p (- j 1)))))
        (if (= (vector-ref jump-table j) 0)
          (vector-set! jump-table j (- j i)))
        (loop (vector-ref suffixes j) i))
       (else
        (vector-set! suffixes (- i 1) (- j 1))
        (loop (- j 1) (- i 1)))))
   (lambda (i)
     (vector-ref jump-table i)))
 
 (define (match t p)
   (define n-p (string-length p))
   (define n-t (string-length t))
   (define good-shift (compute-good-shift-table p))
   (define bad-shift (compute-bad-shift-table p))
   (let loop
     ((i-t 0)
      (i-p (- n-p 1)))
     (cond 
       ((< i-p 0)
        i-t)
       ((> i-t (- n-t n-p))
        #f)
       ((eq? (string-ref t (+ i-t i-p)) (string-ref p i-p))
        (loop i-t (- i-p 1)))
       (else
        (loop (+ i-t (max (good-shift (+ i-p 1))
                          (- (bad-shift (string-ref t (+ i-t i-p))) (+ i-p 1))))
              (- n-p 1)))))))