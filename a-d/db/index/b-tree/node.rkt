#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                          B-Tree Nodes                           *-*-
;-*-*                                                                 *-*-
;-*-*              Theo D'Hondt and Wolfgang De Meuter                *-*-
;-*-*             1993 - 2009 Programming Technology Lab              *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (node)
  (export new delete! read write! type position size capacity complement leaf? meaningless?
          locate-leftmost
          key key! pointer pointer! key-pointer! 
          key-pointer-insert! key-pointer-delete! key-pointer-insert-split!
          borrow-from-left? borrow-from-right? merge!)
  (import (scheme base)
          (a-d file constants)
          (prefix (a-d db rcid) rcid:)
          (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:)
          (prefix (a-d db index b-tree node-type) ntype:))
  (begin
 
    (define-record-type node
      (make t b)
      node?
      (t type)
      (b block))
 
    (define (null-ptr-for node)
      (if (leaf? node)
          rcid:null
          fs:null-block))
 
    (define (new ntyp leaf)
      (define disk (ntype:disk ntyp))
      (define ktyp (ntype:key-type ntyp))
      (define ksiz (ntype:key-size ntyp))
      (define blck (fs:new-block disk))
      (define node (make ntyp blck))
      (define sent (sentinel-for ktyp ksiz))
      (pointer! node 0 (if (not leaf) 1 fs:null-block))
      (do ((null (null-ptr-for node)) ; remains constant ($%^^%$###@ define not allowed after expression)
           (slot 1 (+ slot 1 )))
        ((> slot (capacity node)))
        (key-pointer! node slot sent null))
      node)
 
    (define (delete! node)
      (fs:delete-block (block node)))
 
    (define (position node)
      (disk:position (block node)))
 
    (define (complement slot)
      (if (negative? slot)
          (- -1 slot)
          slot))
 
    (define (leaf? node)
      (fs:null-block? (pointer node 0)))
 
    (define (capacity node)
      (define ntyp (type node))
      (if (leaf? node)
          (ntype:leaf-capacity ntyp)
          (ntype:internal-capacity ntyp)))
 
    (define (size node)
      (define ncap (capacity node))
      (define sent (ntype:key-sent (type node)))
      (define indx (locate-leftmost node sent))
      (if (negative? indx)
          (complement indx)
          ncap))
  
    (define (meaningless? node slot)
      (define ntyp (type node))
      (define ktyp (ntype:key-type ntyp))
      (define sent (ntype:key-sent ntyp))
      (define ===?  (vector-ref equals ktyp))
      (if (= slot (capacity node))
          #t  ; 0 <= slot < cap
          (===? (key node (+ slot 1)) sent)))
 
    (define (read ntyp bptr)
      (define disk (ntype:disk ntyp))
      (define blck (disk:read-block disk bptr))
      (make ntyp blck))
 
    (define (write! node)
      (disk:write-block! (block node)))
 
    (define (key node slot)
      (define ksiz (ntype:key-size (type node)))
      (define ktyp (ntype:key-type (type node)))
      (define blck (block node))
      (define offs (- disk:block-size (* ksiz slot)))
      (define decoder (vector-ref decoders ktyp))
      (decoder blck offs ksiz))
 
    (define (key! node slot skey)
      (define ksiz (ntype:key-size (type node)))
      (define ktyp (ntype:key-type (type node)))
      (define blck (block node))
      (define offs (- disk:block-size (* ksiz slot)))
      (define encoder! (vector-ref encoders ktyp))
      (encoder! blck offs ksiz skey))
 
    (define (pointer node slot)
      (define blck (block node))
      (define rid? (and (not (= slot 0)) (leaf? node)))
      (define pntr-size (if rid? 
                            rcid:size
                            disk:block-ptr-size))
      (define offs (* pntr-size slot))
      (define bptr (disk:decode-fixed-natural blck offs pntr-size))
      (if rid? (rcid:fixed->rcid bptr) bptr))
 
    (define (pointer! node slot pntr)
      (define blck (block node))
      (define rid? (and (not (= slot 0)) (leaf? node)))
      (define pntr-size (if rid? 
                            rcid:size
                            disk:block-ptr-size))
      (define offs (* pntr-size slot))
      (if rid? (set! pntr (rcid:rcid->fixed pntr)))
      (disk:encode-fixed-natural! blck offs pntr-size pntr))
 
    (define (key-pointer! node slot skey pntr)
      (key!     node slot skey)
      (pointer! node slot pntr))
 
    (define (key-pointer-insert! node slot skey pntr)
      (define nsiz (capacity node))
      (define (move index)
        (when (> index slot)
          (key-pointer! node index 
                        (key node (- index 1))
                        (pointer node (- index 1)))
          (move (- index 1))))
      (move nsiz)
      (key-pointer! node slot skey pntr))
 
    (define (key-pointer-delete! node slot)
      (define nsiz (capacity node))
      (define ktyp (ntype:key-type (type node)))
      (define ksiz (ntype:key-size (type node)))
      (define sent (sentinel-for ktyp ksiz))
      (define null (null-ptr-for node))
      (define (move index)
        (when (< index nsiz)
          (key-pointer! node index 
                        (key node (+ index 1))
                        (pointer node (+ index 1)))
          (move (+ index 1))))
      (move slot)
      (key-pointer! node nsiz sent null))
 
    (define (key-pointer-insert-split! node new-node slot skey pntr leaf)
      (define nsiz (capacity node))
      (define ktyp (ntype:key-type (type node)))
      (define ksiz (ntype:key-size (type node)))
      (define sent-skey (sentinel-for ktyp ksiz))
      (define null (null-ptr-for node))
      (define split-slot (+ (quotient (+ nsiz 1) 2) 1))
      (define at-end (> slot nsiz))
      (define hold-key (if at-end skey (key node nsiz)))
      (define hold-datum (if at-end pntr (pointer node nsiz)))
      (define (move slot new-slot)    
        (cond 
          ((<= slot nsiz)
           (key-pointer! new-node new-slot 
                         (key node slot)
                         (pointer node slot))
           (move (+ slot 1) (+ new-slot 1)))
          (else
           new-slot)))
      (define (clear slot)    
        (when (<= slot nsiz)
          (key-pointer! node slot sent-skey null)
          (clear (+ slot 1))))
      (if (not at-end) (key-pointer-insert! node slot skey pntr))
      (let*
          ((prop-key (key node (if leaf (- split-slot 1) split-slot)))
           (insert-slot
            (cond 
              (leaf
               (pointer! new-node 0 fs:null-block)
               (move split-slot 1))
              (else
               (pointer! new-node 0 (pointer node split-slot))
               (move (+ split-slot 1) 1)))))
        (key-pointer! new-node insert-slot hold-key hold-datum)
        (clear split-slot)
        prop-key))
  
    (define (locate-leftmost node skey)
      (define ntyp (type node))
      (define ktyp (ntype:key-type ntyp))
      (define <<<? (vector-ref smaller ktyp))
      (define >>>? (vector-ref greater ktyp))
      (define (search left right)
        (if (> left right)
            right
            (let*
                ((mid (quotient (+ left right) 2))
                 (mid-key (key node mid)))
              (cond
                ((>>>? skey mid-key)
                 (search (+ mid 1) right))
                ((<<<? skey mid-key)
                 (search left (- mid 1)))
                (else
                 (let ((try (search left (- mid 1))))
                   (if (negative? try)
                       try
                       (- mid)))))))) 
      (search 1 (capacity node)))
 
    (define (borrow-from-left? left node pull-skey)
      (define lsiz (size left))
      (define cpty (capacity left)) 
      (if (< (- lsiz 1) (quotient cpty 2)) ; left has enough to give me one?
          #f
          (let* ((lkey (if (leaf? node)
                           (key left lsiz)
                           pull-skey))
                 (lptr (pointer left lsiz))
                 (prop-key (if (leaf? node)
                               (key left (- lsiz 1))
                               lkey))
                 (rptr (if (leaf? node)
                           lptr
                           (pointer node 0))))
            (key-pointer-insert! node 1 lkey rptr)
            (key-pointer-delete! left lsiz)
            (unless (leaf? node)
              (pointer! node 0 lptr))
            prop-key)))
 
    (define (borrow-from-right? node right pull-skey)
      (define rsiz (size right))
      (define cpty (capacity right))
      (define nsiz (size node))
      (if (< (- rsiz 1) (quotient cpty 2))
          #f
          (let* ((rkey (key right 1))
                 (rptr (pointer right 1))
                 (lkey (if (leaf? node)
                           rkey
                           pull-skey))
                 (lptr (if (leaf? node)
                           rptr 
                           (pointer right 0))))
            (key-pointer-insert! node (+ nsiz 1) lkey lptr)
            (key-pointer-delete! right 1)
            (unless (leaf? node)
              (pointer! right 0 rptr))
            rkey)))
 
    (define (merge! accu-node node pull-skey)
      (define cpty (capacity node))
      (define asiz (size accu-node))
      (define nsiz (size node))
      (define strt (if (leaf? node)
                       (+ asiz 1)
                       (+ asiz 2)))
      (if (not (leaf? node))
          (key-pointer! accu-node (+ asiz 1) pull-skey (pointer node 0)))
      (do ((indx strt (+ indx 1)))
        ((= (- indx strt -1) (+ nsiz 1)))
        (key-pointer! accu-node indx (key node 1) (pointer node 1))
        (key-pointer-delete! node 1)))))