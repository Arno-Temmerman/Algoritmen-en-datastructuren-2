#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*               Longest Common Subsequence (Dyn Prog)             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2014  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (needleman-wunsch)
  (export needleman-wunsch)
  (import (scheme base)
          (a-d scheme-tools))
  (begin
 
    ; S12 = similarity matrix; gap = gap penalty
    (define (table rna1 rna2 S12 gap)
      (define F (make-2D-vector (+ 1 (string-length rna1))
                                (+ 1 (string-length rna2))
                                (lambda (i j)
                                  (cond ((= i 0) (* gap j))
                                        ((= j 0) (* gap i))
                                        (else 0)))))
      (do ((i 1 (+ i 1)))
        ((> i (string-length rna1)) F)
        (do ((j 1 (+ j 1)))
          ((> j (string-length rna2)))
          (let ((match (+ (ij? F (- i 1) (- j 1)) (S12 (string-ref rna1 (- i 1))
                                                       (string-ref rna2 (- j 1)))))
                (delete (+ gap (ij? F (- i 1) j)))
                (insert (+ gap (ij? F i (- j 1)))))
            (ij! F i j (max match insert delete))))))
 
    (define (needleman-wunsch rna1 rna2 S12 gap)
      (define F (table rna1 rna2 S12 gap))
      (define align-rna1 "")
      (define align-rna2 "")
      (let loop
        ((i (string-length rna1))
         (j (string-length rna2)))
        (if (or (> i 0) (> j 0))
            (cond ((and (> i 0)
                        (> j 0) 
                        (= (ij? F i j)
                           (+ (ij? F (- i 1) (- j 1)) (S12 (string-ref rna1 (- i 1))
                                                           (string-ref rna2 (- j 1))))))
                   (set! align-rna1 (string-append (string (string-ref rna1 (- i 1))) align-rna1))
                   (set! align-rna2 (string-append (string (string-ref rna2 (- j 1))) align-rna2))
                   (loop (- i 1) (- j 1)))
                  ((and (> i 0)
                        (= (ij? F i j) (+ (ij? F (- i 1) j) gap)))
                   (set! align-rna1 (string-append (string (string-ref rna1 (- i 1))) align-rna1))
                   (set! align-rna2 (string-append "-" align-rna2))
                   (loop (- i 1) j))
                  ((and (> j 0) 
                        (= (ij? F i j) (+ (ij? F i (- j 1)) gap)))
                   (set! align-rna1 (string-append "-" align-rna1))
                   (set! align-rna2 (string-append  (string (string-ref rna2 (- j 1))) align-rna2))
                   (loop i (- j 1))))))
      (cons F (cons align-rna1 align-rna2)))
  
    (define (S c1 c2)
      (cond ((eq? c1 #\A) (cond ((eq? c2 #\A) 2)
                                ((eq? c2 #\G) 0)
                                ((eq? c2 #\C) 0)
                                ((eq? c2 #\T) 0)))
            ((eq? c1 #\G) (cond ((eq? c2 #\A) 0)
                                ((eq? c2 #\G) 2)
                                ((eq? c2 #\C) 0)
                                ((eq? c2 #\T) 0)))
            ((eq? c1 #\C) (cond ((eq? c2 #\A) 0)
                                ((eq? c2 #\G) 0)
                                ((eq? c2 #\C) 2)
                                ((eq? c2 #\T) 0)))
            ((eq? c1 #\T) (cond ((eq? c2 #\A) 0)
                                ((eq? c2 #\G) 0)
                                ((eq? c2 #\C) 0)
                                ((eq? c2 #\T) 2)))))
  
    (define n (needleman-wunsch "ATCAGAGTC" "TTCAGTC" S -2))))
;TTCAG--TC

;> (define t (car n))
;> t
;#(#(0 -2 -4 -6 -8 -10 -12 -14)
;  #(-2 0 -2 -4 -4 -6 -8 -10)
;  #(-4 0 2 0 -2 -4 -4 -6)
;  #(-6 -2 0 4 2 0 -2 -2)
;  #(-8 -4 -2 2 6 4 2 0)
;  #(-10 -6 -4 0 4 8 6 4)
;  #(-12 -8 -6 -2 2 6 8 6)
;  #(-14 -10 -8 -4 0 4 6 8)
;  #(-16 -12 -8 -6 -2 2 6 6)
;  #(-18 -14 -10 -6 -4 0 4 8))