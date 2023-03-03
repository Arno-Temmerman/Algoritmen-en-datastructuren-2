#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*     stack (Vectorial Implementation with positional lists)      *-*-
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
      (make p)
      stack?
      (p plist))
 
    (define (new)
      (make (plist:new eq?)))
 
    (define (push! stack val)
      (plist:add-after! (plist stack) val)
      stack)
 
    (define (top stack)
      (define plst (plist stack))
      (if (= (plist:length plst) 0)
          (error "stack empty (top)" stack))
      (plist:peek plst (plist:last plst)))
 
    (define (pop! stack)
      (define plst (plist stack))
      (if (= (plist:length plst) 0)
          (error "stack empty (pop)" stack))
      (let ((val (plist:peek 
                  plst 
                  (plist:last plst))))
        (plist:delete! 
         plst (plist:last plst))
        val))
 
    (define (empty? stack)
      (plist:empty? (plist stack)))
 
    (define (full? stack)
      (plist:full? (plist stack)))))