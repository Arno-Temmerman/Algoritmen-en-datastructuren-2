#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                            AVL Trees                            *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018 Software Languages Lab                   *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library
  (avl-tree)
  (export new bst? null-tree null-tree?
          empty? full? insert! delete! find
          left left! right right! balance balance! value value!
          root root!)
  (import (scheme base)
          (scheme write))
  (begin
    
    (define balanced 'balanced)
    (define Lhigh 'Lhigh)
    (define Rhigh 'Rhigh)
 
    (define-record-type AVL-node
      (make-AVL-node v l r b)
      AVL-node?
      (v value value!)
      (l left left!)
      (r right right!)
      (b balance balance!))

    (define null-tree '())
 
    (define (null-tree? node)
      (eq? node null-tree))

    (define-record-type bst
      (make r e l)
      bst?
      (r root root!)
      (e equality)
      (l lesser))
 
    (define (new ==? <<?)
      (make null-tree ==? <<?))
 
    (define (single-rotate-left! black)
      (define white (right black))
      (define tree (left white))
      (right! black tree)
      (left! white black)
      white)
 
    (define (single-rotate-right! black)
      (define white (left black))
      (define tree (right white))
      (left! black tree)
      (right! white black)
      white)
 
    (define (double-rotate-left-then-right! black)
      (define white (left black))
      (left! black (single-rotate-left! white))
      (single-rotate-right! black))
 
    (define (double-rotate-right-then-left! black)
      (define white (right black))
      (right! black (single-rotate-right! white))
      (single-rotate-left! black))
 
    (define (single-rotate-left-update! black white)
      (cond ((eq? (balance white) Rhigh)
             (balance! black balanced)
             (balance! white balanced))
            (else
             (balance! black Rhigh)
             (balance! white Lhigh))))

    (define (single-rotate-right-update! black white)
      (cond ((eq? (balance white) Lhigh)
             (balance! black balanced)
             (balance! white balanced))
            (else
             (balance! black Lhigh)
             (balance! white Rhigh))))
 
    (define (double-rotate-right-then-left-update! black white grey)
      (cond ((eq? (balance grey) Lhigh)
             (balance! white Rhigh)
             (balance! black balanced)
             (balance! grey balanced))
            ((eq? (balance grey) balanced)
             (balance! white balanced)
             (balance! black balanced)
             (balance! grey balanced))
            (else
             (balance! white balanced)
             (balance! black Lhigh)
             (balance! grey balanced))))

    (define (double-rotate-left-then-right-update! black white grey)
      (cond ((eq? (balance grey) Rhigh)
             (balance! white Lhigh)
             (balance! black balanced)
             (balance! grey balanced))
            ((eq? (balance grey) balanced)
             (balance! white balanced)
             (balance! black balanced)
             (balance! grey balanced))
            (else
             (balance! white balanced)
             (balance! black Rhigh)
             (balance! grey balanced))))
 
    (define (check-after-insert-left parent child! black)
      (cond 
        ((eq? (balance black) Rhigh)
         (balance! black balanced)
         #f)
        ((eq? (balance black) balanced)
         (balance! black Lhigh)
         #t)
        (else ; child already was left-high
         (let* ((white (left black))
                (grey  (right white))) 
           (if (eq? (balance white) Lhigh)
               (begin
                 (child! parent (single-rotate-right! black))
                 (single-rotate-right-update! black white))
               (begin 
                 (child! parent (double-rotate-left-then-right! black))
                 (double-rotate-left-then-right-update! black white grey)))
           #f))))
 
    (define (check-after-insert-right parent child! black)
      (cond
        ((eq? (balance black) Lhigh)
         (balance! black balanced)
         #f)
        ((eq? (balance black) balanced)
         (balance! black Rhigh)
         #t)
        (else ; child already was right-high
         (let* ((white (right black))
                (grey  (left white)))
           (if (eq? (balance white) Rhigh)
               (begin
                 (child! parent (single-rotate-left! black))
                 (single-rotate-left-update! black white))
               (begin 
                 (child! parent (double-rotate-right-then-left! black))
                 (double-rotate-right-then-left-update! black white grey)))
           #f))))
 
    (define (check-after-delete-left parent child! black)
      (cond 
        ((eq? (balance black) Lhigh)
         (balance! black balanced) 
         #t)
        ((eq? (balance black) balanced)
         (balance! black Rhigh) 
         #f)
        (else ; right-high
         (let* ((white (right black)) 
                (white-bal (balance white))
                (grey (left white)))
           (if (or (eq? white-bal balanced)
                   (eq? white-bal Rhigh))
               (begin 
                 (child! parent (single-rotate-left! black))
                 (single-rotate-left-update! black white))
               (begin
                 (child! parent (double-rotate-right-then-left! black))
                 (double-rotate-right-then-left-update! black white grey)))
           (not (eq? white-bal balanced))))))
 
    (define (check-after-delete-right parent child! black)
      (cond 
        ((eq? (balance black) Rhigh)
         (balance! black balanced)
         #t)
        ((eq? (balance black) balanced)
         (balance! black Lhigh) 
         #f)
        (else
         (let* ((white (left black)) 
                (white-bal (balance white))
                (grey (right white)))
           (if (or (eq? white-bal Lhigh)
                   (eq? white-bal balanced))
               (begin
                 (child! parent (single-rotate-right! black))
                 (single-rotate-right-update! black white))
               (begin
                 (child! parent (double-rotate-left-then-right! black))
                 (double-rotate-left-then-right-update! black white grey)))
           (not (eq? white-bal balanced))))))
 
    (define (empty? avl)
      (null-tree? (root avl)))
 
    (define (full? avl)
      #f)
 
    (define (insert! avl val)
      (define <<? (lesser avl))
      (define ==? (equality avl))
   
      (let insert-rec
        ((parent null-tree)
         (child! (lambda (ignore child) (root! avl child)))
         (child (root avl)))
        (cond
          ((null-tree? child)
           (child! parent (make-AVL-node val null-tree null-tree balanced))
           #t)
          ((<<? (value child) val)
           (if (insert-rec child right! (right child))
               (check-after-insert-right parent child! child)
               #f))
          ((<<? val (value child))
           (if (insert-rec child left! (left child))
               (check-after-insert-left parent child! child) 
               #f))
          (else  ; value = (AVL-node-value node)
           (value! child val)
           #f)))
      avl)
 
    (define (find avl key)
      (define <<? (lesser avl))
      (define ==? (equality avl))
      (let find-value 
        ((node (root avl)))
        (if (null-tree? node)
            #f
            (let ((node-val (value node)))
              (cond
                ((==? (value node) key)
                 node-val)
                ((<<? key node-val)
                 (find-value (left node)))
                ((<<? node-val key)
                 (find-value (right node)))
                (else
                 #f))))))
 
    (define (delete! avl val)
      (define ==? (equality avl))
      (define <<? (lesser avl))
   
      (define (find-leftmost deleted parent child! child)
        (if (null-tree? (left child))
            (begin
              (value! deleted (value child))
              (child! parent (right child))
              #t)
            (if (find-leftmost deleted child left! (left child))
                (check-after-delete-left parent child! child)
                #f)))
   
      (define (delete-node parent child! child)
        (cond 
          ((null-tree? (left child))
           (child! parent (right child))
           #t)
          ((null-tree? (right child))
           (child! parent (left child))
           #t)
          (else
           (if (find-leftmost child child right! (right child))
               (check-after-delete-right parent child! child)
               #f))))
   
      (let find-node
        ((parent null-tree)
         (child! (lambda (ignore child) (root! avl child)))
         (child (root avl)))
        (cond
          ((null-tree? child)
           #f)
          ((==? (value child) val)
           (delete-node parent child! child))
          ((<<? (value child) val)
           (if (find-node child right! (right child))
               (check-after-delete-right parent child! child)
               #f))
          ((<<? val (value child))
           (if (find-node child  left! (left child))
               (check-after-delete-left parent child! child)
               #f))))
      avl)))