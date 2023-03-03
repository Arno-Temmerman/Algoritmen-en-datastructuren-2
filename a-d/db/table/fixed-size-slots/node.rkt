#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                      Relational Table Nodes                     *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


(define-library (node)
  (export new read write! record record! delete!
          next next! previous previous! position schema
          clear-slot! slot-occupied? all-free? all-occupied?)
  (import (prefix (a-d disk config) disk:)
          (a-d file constants)
          (prefix (a-d disk file-system) fs:)
          (prefix (a-d db table fixed-size-slots schema) scma:)
          (scheme base)
          (only (rnrs arithmetic bitwise-6) bitwise-not bitwise-and bitwise-ior))
  (begin

    ; Composition of a table block in central memory

    (define-record-type node
      (make s b)
      node?
      (s schema)
      (b block))
 
    ; Composition of a table block no the disk
 
    (define next-offset 0)                                       ; location of the next-block pointer
    (define prev-offset (+ next-offset disk:block-ptr-size))     ; location of the previous-block pointer
    (define bits-offset (+ prev-offset disk:block-ptr-size))     ; location of the occupancy bits
 
    (define (next node)
      (define blck (block node))
      (disk:decode-fixed-natural blck next-offset disk:block-ptr-size))
    (define (next! node next)
      (define blck (block node))
      (disk:encode-fixed-natural! blck next-offset disk:block-ptr-size next))
    (define (previous node)
      (define blck (block node))
      (disk:decode-fixed-natural blck prev-offset disk:block-ptr-size))
    (define (previous! node prev)
      (define blck (block node))
      (disk:encode-fixed-natural! blck prev-offset disk:block-ptr-size prev))
    (define (occupancy-bits node)
      (define blck (block node))
      (define scma (schema node))
      (define byts (scma:nr-of-occupancy-bytes scma))
      (disk:decode-fixed-natural blck bits-offset byts))
    (define (occupancy-bits! node bits)
      (define blck (block node))
      (define scma (schema node))
      (define byts (scma:nr-of-occupancy-bytes scma))
      (disk:encode-fixed-natural! blck bits-offset byts bits))
 
    (define (new scma next)
      (define blck (fs:new-block (scma:disk scma)))
      (define node (make scma blck))
      (next!     node next)
      (previous! node fs:null-block)
      (occupancy-bits! node 0)
      node)
 
    (define (delete! node)
      (fs:delete-block (block node)))
 
    (define (read scma bptr)
      (define disk (scma:disk scma))
      (define blck (disk:read-block disk bptr))
      (make scma blck))
 
    (define (write! node)
      (define blck (block node))
      (disk:write-block! blck))
 
    (define (position node)
      (disk:position (block node)))
 
    (define (calc-record-offset node slot)
      (define scma (schema node))
      (define skip (+ scma:fixed-header-size (scma:nr-of-occupancy-bytes scma)))
      (define rsiz (scma:record-size scma))
      (+ skip (* rsiz slot)))
 
    (define (record! node slot tupl)
      (define scma (schema node))
      (define blck (block node))
      (let loop
        ((cntr 0)
         (offs (calc-record-offset node slot))
         (vals tupl))
        (cond ((null? vals)
               (if (< cntr (scma:nr-of-attributes scma))
                   (error "too few values in tuple" vals)))
              ((>= cntr (scma:nr-of-attributes scma))
               (if (not (null? vals))
                   (error "too many values in tuple" vals)))
              (else ((vector-ref encoders (scma:type scma cntr)) 
                     blck offs (scma:size scma cntr) (car vals))
                    (loop (+ cntr 1) (+ offs (scma:size scma cntr)) (cdr vals)))))
      (occupy-slot! node slot))
 
    (define (record node slot)
      (define scma (schema node))
      (define blck (block node))
      (let loop
        ((cntr 0)
         (offs (calc-record-offset node slot)))
        (if (= cntr (scma:nr-of-attributes scma))
            '()
            (cons ((vector-ref decoders (scma:type scma cntr)) 
                   blck offs (scma:size scma cntr))
                  (loop (+ cntr 1) (+ offs (scma:size scma cntr)))))))
 
    (define (occupy-slot! node slot)
      (define bits (occupancy-bits node))
      (occupancy-bits! node (bitwise-ior bits (expt 2 slot))))
 
    (define (clear-slot! node slot)
      (define bits (occupancy-bits node))
      (occupancy-bits! node (bitwise-and bits (bitwise-not (expt 2 slot)))))
 
    (define (slot-occupied? node slot)
      (define bits (occupancy-bits node))
      (not (= (bitwise-and bits (expt 2 slot)) 0)))
 
    (define (all-free? node)
      (define bits (occupancy-bits node))
      (= bits 0))
 
    (define (all-occupied? node)
      (define bits (occupancy-bits node))
      (define all1 (- (expt 2 (scma:capacity (schema node))) 1))
      (= (bitwise-and bits all1) all1))))