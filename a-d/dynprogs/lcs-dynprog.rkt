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

(define-library (lcs-dynprog)
  (export lcs)
  (import (scheme base)
          (a-d scheme-tools))
  (begin
 
    (define (tables str1 str2)
      (define dst (make-2D-vector (+ 1 (string-length str1))
                                  (+ 1 (string-length str2))
                                  (lambda (i j) 0)))
      (define dir (make-2D-vector (+ 1 (string-length str1))
                                  (+ 1 (string-length str2))
                                  (lambda (i j) "**")))
      (do ((i 1 (+ i 1)))
        ((> i (string-length str1)) (cons dst dir))
        (do ((j 1 (+ j 1)))
          ((> j (string-length str2)))
          (cond ((eq? (string-ref str1 (- i 1))
                      (string-ref str2 (- j 1)))
                 (ij! dst i j (+ 1 (ij? dst (- i 1) (- j 1))))
                 (ij! dir i j "<^"))
                ((<= (ij? dst i (- j 1)) (ij? dst (- i 1) j))
                 (ij! dst i j (ij? dst (- i 1) j))
                 (ij! dir i j " ^"))
                (else
                 (ij! dst i j (ij? dst i (- j 1)))
                 (ij! dir i j "< "))))))
 
    (define (read-lcs str1 str2 dir)
      (define (read-rec i j)
        (define s (ij? dir i j))
        (cond ((or (= i 0) (= j 0))
               "")
              ((string=? (ij? dir i j) "<^")
               (string-append (read-rec (- i 1) (- j 1))
                              (string (string-ref str1 (- i 1)))))
              ((string=? (ij? dir i j) " ^")
               (read-rec (- i 1) j))
              (else
               (read-rec i (- j 1)))))
      (read-rec (string-length str1) 
                (string-length str2)))
 
    (define (lcs str1 str2)
      (define ts (tables str1 str2))
      (read-lcs str1 str2 (cdr ts)))
 
    ;(lcs "HUMAN" "CHIMPANZEE")
    ;    ['H', 'M', 'A', 'N']
 
    ;(lcs "ABCBDAB" "BDCABA" )
    ;    ['B', 'C', 'B', 'A']
    )
  )