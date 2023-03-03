#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*      stack (Linked Implementation with positional-list)         *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                  2011  Software Languages Lab                   *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (stack)
  (export new stack? push! pop! top empty? full?)
  (import (scheme base)
          (prefix (a-d positional-list adt) plist:))
  (begin
 
    (define-record-type stack
      (make l)
      stack?
      (l plist))
 
    (define (new)
      (make (plist:new eq?)))
 
    (define (push! stack val)
      (plist:add-before! (plist stack) val)
      stack)
 
    (define (top stack)
      (define plst (plist stack))
      (if (= (plist:length plst) 0)
          (error "stack empty (top)"stack))
      (plist:peek plst (plist:first plst)))
 
    (define (pop! stack)
      (define plst (plist stack))
      (define first-position (plist:first plst))
      (if (= (plist:length plst) 0)
          (error "stack empty (pop)" stack))
      (let ((val (plist:peek plst first-position)))
        (plist:delete! plst first-position)
        val))
 
    (define (empty? stack)
      (define plst (plist stack))
      (plist:empty? plst))
 
    (define (full? stck)
      (define plst (plist stck))
      (plist:full? plst))))