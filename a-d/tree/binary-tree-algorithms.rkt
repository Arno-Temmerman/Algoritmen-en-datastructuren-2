#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                     Binary Tree Algorithms                      *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (binary-tree-algorithms)
  (export in-order iterative-in-order
          post-order iterative-post-order
          pre-order iterative-pre-order
          breadth-first)
  (import
   (scheme base)
   (a-d tree binary-tree)
   (prefix (a-d stack linked) stack:)
   (prefix (a-d queue linked) queue:))
  (begin
   
    (define (in-order tree proc)
      (define (do-traverse current)
        (when (not (null-tree? current))
          (do-traverse (left current))
          (proc (value current))
          (do-traverse (right current))))
      (do-traverse tree))
 
    (define (iterative-in-order tree proc)
      (define stack (stack:new))
      (define (loop-up)
        (let ((node (stack:pop! stack)))
          (proc (value node))
          (if (not (null-tree? (right node)))
              (begin (stack:push! stack (right node))
                     (loop-down))
              (if (not (stack:empty? stack))
                  (loop-up)))))
      (define (loop-down)
        (let ((node (stack:top stack)))
          (if (not (null-tree? (left node)))
              (begin (stack:push! stack (left node))
                     (loop-down))
              (loop-up))))
      (stack:push! stack tree)
      (loop-down))
  
    (define (pre-order tree proc)
      (define (do-traverse current)
        (when (not (null-tree? current))
          (proc (value current))
          (do-traverse (left current))
          (do-traverse (right current))))
      (do-traverse tree))
 
    (define (iterative-pre-order tree proc)
      (define stack (stack:new))
      (define (loop)
        (if (not (stack:empty? stack))
            (let ((node (stack:pop! stack)))
              (proc (value node))
              (if (not (null-tree? (right node)))
                  (stack:push! stack (right node)))
              (if (not (null-tree? (left node)))
                  (stack:push! stack (left node)))
              (loop))))
      (stack:push! stack tree)
      (loop))
 
    (define (post-order tree proc)
      (define (do-traverse current)
        (when (not (null-tree? current))
          (do-traverse (left current))
          (do-traverse (right current))
          (proc (value current))))
      (do-traverse tree))
 
    (define (breadth-first tree proc)
      (define q (queue:new))
      (define (loop)
        (let ((node (queue:serve! q)))
          (proc (value node))
          (if (not (null-tree? (left node)))
              (queue:enqueue! q (left node)))
          (if (not (null-tree? (right node)))
              (queue:enqueue! q (right node)))
          (if (not (queue:empty? q))
              (loop))))
      (queue:enqueue! q tree)
      (loop))
 
    (define (iterative-post-order tree proc)
      (define stack (stack:new))
      (define (loop-up-right)
        (let ((node (stack:pop! stack)))
          (proc (value node))
          (cond ((and (not (stack:empty? stack))
                      (eq? (right (stack:top stack)) node))
                 (loop-up-right))
                ((not (stack:empty? stack))
                 (loop-up-left)))))
      (define (loop-up-left)
        (let ((node (stack:pop! stack)))
          (cond ((not (null-tree? (right node)))
                 (stack:push! stack node)
                 (stack:push! stack (right node))
                 (loop-down))
                ((and (not (stack:empty? stack))
                      (eq? (right (stack:top stack)) node))
                 (proc (value node))
                 (loop-up-right))
                ((not (stack:empty? stack))
                 (proc (value node))
                 (loop-up-left)))))
      (define (loop-down)
        (if (not (stack:empty? stack))
            (let ((node (stack:top stack)))
              (if (null-tree? (left node))
                  (loop-up-left)
                  (begin
                    (stack:push! stack (left node))
                    (loop-down))))))
      (stack:push! stack tree)
      (loop-down))))