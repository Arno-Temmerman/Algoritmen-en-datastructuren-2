#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Longest Common Subsequence (Naive)               *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2014  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (lcs-naive)
  (export lcs)
  (import (scheme base))
  (begin
   
    (define (longest str1 str2)
      (if (< (string-length str1) (string-length str2))
          str2 str1))
 
    (define (lcs str1 str2)
      (define l1 (string-length str1))
      (define l2 (string-length str2))
      (if (or (= l1 0)
              (= l2 0))
          ""
          (let ((str1-start (substring str1 0 (- l1 1)))
                (str2-start (substring str2 0 (- l2 1)))
                (last-char1 (string-ref str1 (- l1 1)))
                (last-char2 (string-ref str2 (- l2 1))))
            (if (eq? last-char1 last-char2)
                (string-append (lcs str1-start str2-start) (string last-char1))
                (longest (lcs str1 str2-start) 
                         (lcs str1-start str2))))))
 
    ;(lcs "HUMAN" "CHIMPANZEE")
    ;    ['H', 'M', 'A', 'N']
    )
  )