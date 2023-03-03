#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                   Longest Common Subsequence                    *-*-
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
          (scheme write))
  (begin   
    
    (define (lcs str1 str2)
      (if (or (= (string-length str1) 0)
              (= (string-length str2) 0))
          ""
          (let ((str1-start (substring str1 0 (- (string-length str1) 1)))
                (str2-start (substring str2 0 (- (string-length str2) 1)))
                (str1-end   (substring str1 (- (string-length str1) 1) (string-length str1)))
                (str2-end   (substring str2 (- (string-length str2) 1) (string-length str2))))
            (if (string=? str1-end str2-end)
                (string-append (lcs str1-start str2-start) str1-end)
                (let ((lcs1 (lcs str1 str2-start))
                      (lcs2 (lcs str1-start str2)))
                  (if (< (string-length lcs1) (string-length lcs2))
                      lcs2
                      lcs1))))))
 
    ;(lcs "HUMAN" "CHIMPANZEE")
    ;    ['H', 'M', 'A', 'N']
    )
  )