#lang r7rs
(import (scheme base)
        (scheme write))

; A short intermezzo on continuations.

; Continuations allow you to "grab the future of an expression",
; which is useful to implement several control mechanisms in a programming language,
; for example: Exception Handling (throwing and catching errors).

; Grabbing this future in Scheme can be done by the procedure
; call-with-current-continuation, or the shorthand call/cc. 
; call/cc takes a single-argument lambda as input.
; The argument of this lambda represents the "future" of
; calculations that will follow after the execution of its body.

; Below is an example of a simple calculation where we use call/cc in between
; to assign the continuation to a variable
(define *my-future* '())
(* 10 (+ 1 (call/cc (lambda (future) ; future represents the continuation
                      (set! *my-future* future)
                      1))))

;; Run this file and check in your REPL what *my-future* now holds by calling it with an argument
;; e.g. (*my-future* 5)

;; You should notice that *my-future* now represents a lambda equivalent to the expression surrounding the call/cc execution,
;; i.e. (lambda (x) (* 10 (+ 1 x)))

;; In this and the following chapters of the course, call/cc will be used to create return-statements in Scheme.
;; This can be simply realised by surrounding the last expression of your body with a call/cc statement.
;; Below a simple example of such a procedure:
(define (find-3 lst)
  (call/cc (lambda (return) ; since no expressions follow call/cc, the continuation refers to the "end" of this procedure.
             (display "Start finding 3 in given list")(newline)
             (for-each (lambda (x)
                         (display x)
                         (if (= x 3)
                             (return (display 'found)))) ; calling the continuation allows us to "jump" to it, in this case it means the end of the procedure.
                       lst)
             (display 'not-found))))
;; Try the procedure with the list '(1 2 3 4 5).
(find-3 '(1 2 3 4 5))(newline)

;; You should notice that the elements 4 and 5 will never be displayed,
;; as we "jump out of the for-each expression" once we find the number 3.

;; Also give it a try with a list not containing the number 3
(find-3 '(1 2 4 5))

;; Do note that this is the easiest application of call/cc.
;; It can be used for many different and more complicated features.
;; The interested reader can have a look at the following files in the a-d/examples:
;; coroutines.rkt, coroutines2.rkt and try-catch.rkt 