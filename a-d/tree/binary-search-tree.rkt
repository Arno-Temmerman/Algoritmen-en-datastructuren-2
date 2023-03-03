#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Binary Search Trees                        *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (binary-search-tree)
  (export new bst? find insert! delete! empty? full? root)
  (import (prefix (a-d tree binary-tree) tree:)
          (scheme base)
          (scheme write))
  (begin 
    (define-record-type bst
      (make r e l)
      bst?
      (r root root!)
      (e equality)
      (l lesser))

    (define (new ==? <<?)
      (make tree:null-tree ==? <<?))
 
    (define (find bst key)
      (define <<? (lesser bst))
      (define ==? (equality bst))
      (let find-key 
        ((node (root bst)))
        (if (tree:null-tree? node)
            #f
            (let 
                ((node-value (tree:value node)))
              (cond 
                ((==? node-value key)
                 node-value)
                ((<<? node-value key)
                 (find-key (tree:right node)))
                ((<<? key node-value)
                 (find-key (tree:left node))))))))
 
    (define (insert! bst val)
      (define <<? (lesser bst))
      (let insert-iter 
        ((parent tree:null-tree)
         (child! (lambda (ignore child) (root! bst child)))
         (child (root bst)))
        (cond 
          ((tree:null-tree? child)
           (child! parent 
                   (tree:new val 
                             tree:null-tree 
                             tree:null-tree)))
          ((<<? (tree:value child) val)
           (insert-iter child tree:right! 
                        (tree:right child)))
          ((<<? val (tree:value child))
           (insert-iter child tree:left!
                        (tree:left child)))
          (else 
           (tree:value! child val)))))
 
    (define (delete! bst val)
      (define <<? (lesser bst))
      (define ==? (equality bst))
      (define (find-leftmost deleted parent child! child)
        (if (tree:null-tree? (tree:left child))
            (begin 
              (tree:value! deleted (tree:value child))
              (child! parent (tree:right child)))
            (find-leftmost deleted child 
                           tree:left!
                           (tree:left child))))
      (define (delete-node parent child! child)
        (cond 
          ((tree:null-tree? (tree:left child))
           (child! parent (tree:right child)))
          ((tree:null-tree? (tree:right child))
           (child! parent (tree:left child)))
          (else
           (find-leftmost child
                          child 
                          tree:right! 
                          (tree:right child)))))
      (let find-node
        ((parent tree:null-tree)
         (child! (lambda (ignore child) (root! bst child)))
         (child (root bst)))
        (cond 
          ((tree:null-tree? child)
           #f)
          ((==? (tree:value child) val)
           (delete-node parent child! child)
           (tree:value child))
          ((<<? (tree:value child) val)
           (find-node child tree:right! (tree:right child)))
          ((<<? val (tree:value child))
           (find-node child tree:left! (tree:left child))))))
 
    (define (empty? bst)
      (tree:null-tree? (root bst)))
 
    (define (full? bst)
      #f)))