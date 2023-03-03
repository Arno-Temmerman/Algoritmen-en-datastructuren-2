#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                        DIMACS CNF Parser                        *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

; Inspired on the Racket script made by Guannan Wei

(define-library (dimacs-parser)
  (export parse-dimacs-file)
  (import (scheme base)
          (scheme file)
          (only (racket string) string-split)
          (only (racket mpair) list->mlist) ; string-split returns a Racket list which is incompatible with R7RS lists
          (a-d sat cnf)
          (a-d scheme-tools))
  (begin

    (define (non-empty-string? x)
      (and (string? x) (not (zero? (string-length x)))))
 
    (define (all-but-last l)
      (cond ((null? l) (error "all-but-last: empty list" l))
            ((null? (cdr l)) '())
            (else (cons (car l) (all-but-last (cdr l))))))

    (define (read-lines port)
      (if (input-port? port)
          (let loop ()
            (if (eof-object? (peek-char port))
                '()
                (cons (read-line port) (loop))))
          (error "read-lines: provided port is not an input port" port)))

    (define (parse in-port)
      (define var-ctr #f)
      (define clause-ctr #f)

      (define (parse-literal str)
        (let ((n (string->number str)))
          (make-literal (abs n) (< n 0))))

      (define (parse-clause line)
        (let ((lits (map parse-literal (all-but-last (list->mlist (string-split line))))))
          (make-clause lits (length lits))))
   
      (define (parse-header line)
        (if (char=? (string-ref line 0) #\p)
            (let ((words (list->mlist (string-split line))))
              (if (and (= (length words) 4)
                       (string=? (list-ref words 1) "cnf"))
                  (begin (set! var-ctr (string->number (list-ref words 2)))
                         (set! clause-ctr (string->number (list-ref words 3))))
                  (error "parse-header: incorrect header line format" line)))
            (error "parse-header: line doesn't start with char 'p'" line)))
   
      (let ((clauses (fold-right
                      (lambda (line acc) 
                        (if (non-empty-string? line)
                            (cond ((char=? (string-ref line 0) #\c) acc) ; skip irrelevant lines
                                  ((char=? (string-ref line 0) #\%) acc)
                                  ((char=? (string-ref line 0) #\#) acc)
                                  ((char=? (string-ref line 0) #\0) acc)
                                  ((char=? (string-ref line 0) #\p) (parse-header line) acc)
                                  (else (cons (parse-clause line) acc)))
                            acc))
                      '()
                      (read-lines in-port))))
        (if (and var-ctr
                 (= (length clauses) clause-ctr))
            (make-formula clauses var-ctr clause-ctr)
            (error "parse: something went wrong during parsing" var-ctr clause-ctr (length clauses)))))

    (define (parse-dimacs-file filename)
      (call-with-input-file filename parse #:mode 'text))))