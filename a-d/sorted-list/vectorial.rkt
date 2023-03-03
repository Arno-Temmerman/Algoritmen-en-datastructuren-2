#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Sorted Lists (Vectorial Implementation)             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2018  Software Languages Lab                  *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (linked-sorted-list)
  (export new from-scheme-list sorted-list? empty? full? length
          find! delete! add! peek
          set-current-to-first! set-current-to-next! has-current? current-has-next?)
  (import (except (scheme base) length))
  (begin
    (define default-size 20)

    (define-record-type sorted-list
      (make-sorted-list s c v l e)
      sorted-list?
      (s size size!)
      (c current current!)
      (v storage)
      (l lesser)
      (e equality))
 
    (define (make len <<? ==?)
      (make-sorted-list 0 -1 (make-vector (max default-size len)) <<? ==?))
 
    (define (new <<? ==?)
      (make 0 <<? ==?))
 
    (define (from-scheme-list slst <<? ==?)
      (let loop
        ((lst slst)
         (idx 0))
        (if (null? lst)
            (make idx <<? ==?)
            (add! (loop (cdr lst) (+ idx 1)) (car lst)))))
  
    (define (storage-move-right vector i j)
      (define (iter idx)
        (vector-set! vector (+ idx 1) (vector-ref vector idx))
        (if (> idx i)
            (iter (- idx 1))))
      (iter j))
    (define (storage-move-left vector i j)
      (define (iter idx)
        (vector-set! vector (- idx 1) (vector-ref vector idx))
        (if (< idx j)
            (iter (+ idx 1))))
      (iter i))
 
    (define (length slst)
      (size slst))
 
    (define (empty? slst)
      (= (length slst) 0))
 
    (define (full? slst)
      (= (length slst)
         (vector-length (storage slst))))
 
    (define (find-sequential! slst key)
      (define ==? (equality slst))
      (define <<? (lesser slst))
      (define vect (storage slst))
      (define leng (size slst))
      (let sequential-search
        ((curr 0))
        (cond ((>= curr leng)
               (current! slst -1))
              ((==? key (vector-ref vect curr))
               (current! slst curr))
              ((<<? (vector-ref vect curr) key)
               (sequential-search (+ curr 1)))
              (else
               (current! slst -1))))
      slst)
 
    (define (find! slst key)
      (define ==? (equality slst))
      (define <<? (lesser slst))
      (define vect (storage slst))
      (define leng (size slst))
      (let binary-search
        ((left 0)
         (right (- leng 1)))
        (if (<= left right)
            (let ((mid (quotient (+ left right 1) 2)))
              (cond
                ((==? (vector-ref vect mid) key)
                 (current! slst mid))
                ((<<? (vector-ref vect mid) key)
                 (binary-search (+ mid 1) right))
                (else
                 (binary-search left (- mid 1)))))
            (current! slst -1)))
      slst)
 
    (define (delete! slst)
      (define vect (storage slst))
      (define last (size slst))
      (define curr (current slst))
      (if (not (has-current? slst))
          (error "no current (delete!)" slst))
      (if (< (+ curr 1) last)
          (storage-move-left vect (+ curr 1) last))
      (size! slst (- last 1))
      (current! slst -1)
      slst)
 
    (define (peek slst)
      (if (not (has-current? slst))
          (error "no current (peek)" slst)
          (vector-ref (storage slst) (current slst))))
 
    (define (add! slst val)
      (define <<? (lesser slst))
      (define vect (storage slst))
      (define leng (size slst))
      (if (= leng (vector-length vect))
          (error "list full (add!)" slst))
      (let vector-iter
        ((idx leng))
        (cond 
          ((= idx 0)
           (vector-set! vect idx val))
          ((<<? val (vector-ref vect (- idx 1)))
           (vector-set! vect idx (vector-ref vect (- idx 1)))
           (vector-iter (- idx 1)))
          (else
           (vector-set! vect idx val))))
      (size! slst (+ leng 1))
      slst)
 
    (define (set-current-to-first! slst)
      (current! slst 0))
 
    (define (set-current-to-next! slst)
      (if (not (has-current? slst))
          (error "current has no meaningful value (set-current-to-next!" slst)
          (current! slst (+ 1 (current slst)))))
 
    (define (has-current? slst)
      (not (= -1 (current slst))))
 
    (define (current-has-next? slst)
      (if (not (has-current? slst))
          (error "no Current (current-has-next?)" slst)
          (< (+ (current slst) 1) (length slst))))))