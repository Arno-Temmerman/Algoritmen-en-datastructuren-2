#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                           B-Trees                               *-*-
;-*-*                                                                 *-*-
;-*-*               Theo D'Hondt and Wolfgang De Meuter               *-*-
;-*-*               1993 - 2010 Software Languages Lab                *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (b-tree)
  (export new open flush! drop! b-tree? name
          insert! delete! peek update!
          find! set-current-to-first! set-current-to-next! 
          print
          tree-root tree-root! node-type node-type! path)
  (import (scheme base)
          (scheme write)
          (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:)
          (a-d file constants)
          (prefix (a-d db index b-tree node) node:)
          (prefix (a-d db index b-tree path) path:)
          (prefix (a-d db index b-tree node-type) ntype:))
  (begin
 
    (define-record-type b-tree
      (make n h p t)
      b-tree?
      (n name name!)
      (h header header!)
      (p path path!)
      (t node-type node-type!))
 
    (define keytype-size 1) ; we use one byte to encode the key's type
    (define keysize-size 2) ; we use two bytes to encode the key's size
 
    (define key-type-offset   0)
    (define key-size-offset   (+ key-type-offset keytype-size))
    (define root-block-offset (+ key-size-offset keysize-size))
 
    (define (key-type indx)
      (define hder (header indx))
      (disk:decode-fixed-natural hder key-type-offset keytype-size))
    (define (key-type! indx ktyp)
      (define hder (header indx))
      (disk:encode-fixed-natural! hder key-type-offset keytype-size ktyp))
    (define (key-size indx)
      (define hder (header indx))
      (disk:decode-fixed-natural hder key-size-offset keysize-size))
    (define (key-size! indx ksiz)
      (define hder (header indx))
      (disk:encode-fixed-natural! hder key-size-offset keysize-size ksiz))
    (define (tree-root indx)
      (define hder (header indx))
      (disk:decode-fixed-natural hder root-block-offset disk:block-ptr-size))
    (define (tree-root! indx root)
      (define hder (header indx))
      (disk:encode-fixed-natural! hder root-block-offset disk:block-ptr-size root))     
 
    (define (new disk name ktyp ksiz) 
      (define ntyp (ntype:new disk ktyp ksiz))
      (define stck (path:new))
      (define hder (fs:new-block disk))
      (define tree (make name hder stck ntyp))
      (key-size!  tree ksiz)
      (key-type!  tree ktyp)
      (tree-root! tree fs:null-block)
      (fs:mk disk name (disk:position hder))
      (disk:write-block! hder)
      tree)
 
    (define (open disk name) 
      (define hptr (fs:whereis disk name))
      (define stck (path:new))
      (define hder (disk:read-block disk hptr))
      (define tree (make name hder stck '()))
      (define ksiz (key-size tree))
      (define ktyp (key-type tree))
      (define ntyp (ntype:new disk ktyp ksiz))
      (node-type! tree ntyp)
      tree)

    (define (find! tree key)
      (define stck (path tree))
      (define (build-path bptr)
        (define node (node:read (node-type tree) bptr))
        (define slot (node:locate-leftmost node key))
        (define actual-slot (node:complement slot))
        (cond
          ((node:leaf? node)
           (cond 
             ((negative? slot)
              (path:push! stck node (+ actual-slot 1))
              done)
             ((node:meaningless? node actual-slot)
              (path:clear! stck)        
              not-found)
             (else
              (path:push! stck node (+ actual-slot 1))
              next-higher)))
          (else
           (path:push! stck node actual-slot)
           (build-path (node:pointer node actual-slot)))))
      (path:clear! stck)        
      (if (fs:null-block? (tree-root tree))
          not-found
          (build-path (tree-root tree))))
 
    (define (insert! tree key rcid)
      (define ntyp (node-type tree))
      (define ktyp (ntype:key-type ntyp))
      (define sent (ntype:key-sent ntyp))
      (define stck (path tree))
      (define root (tree-root tree))
      (define (build-path bptr)
        (define node (node:read ntyp bptr))
        (define slot (node:locate-leftmost node key))
        (path:push! stck node (node:complement slot))
        (if (not (node:leaf? node))
            (build-path (node:pointer node (node:complement slot)))))
      (define (traverse-path key pointer leaf?)  
        (if (path:empty? stck)
            (let 
                ((new-root (node:new ntyp leaf?)))
              (node:key-pointer! new-root 1 key pointer)
              (node:pointer! new-root 0 root) 
              (node:write! new-root)
              (tree-root! tree (node:position new-root)))
            (let* 
                ((node (path:node stck))
                 (slot (path:slot stck))
                 (boundary (node:locate-leftmost node sent)))
              (path:pop! stck)
              (cond
                ((negative? boundary)
                 (node:key-pointer-insert! node (+ slot 1) key pointer)
                 (node:write! node))
                (else
                 (let*
                     ((new-node (node:new ntyp leaf?))
                      (prop-key 
                       (node:key-pointer-insert-split! 
                        node new-node (+ slot 1) key pointer leaf?)))
                   (node:write! node)
                   (node:write! new-node)
                   (traverse-path prop-key (node:position new-node) #f)))))))
      (path:clear! stck)
      (if (not (fs:null-block? root))
          (build-path root))
      (traverse-path key rcid #t)
      (path:clear! stck)
      done)
 
    (define (set-current-to-next! tree)
      (define stck (path tree))
      (define ntyp (node-type tree))
      (define (backtrack level)
        (define node (path:node stck))
        (define slot (path:slot stck))
        (path:pop! stck)
        (if (node:meaningless? node slot)
            (if (path:empty? stck)
                not-found
                (backtrack (+ level 1)))
            (advance node (+ slot 1) level)))
      (define (advance node slot level)
        (path:push! stck node slot)
        (if (= 0 level) 
            done
            (let*
                ((bptr (node:pointer node slot))
                 (next-node (node:read ntyp bptr)))
              (if (= level 1) 
                  (advance next-node 1 0)
                  (advance next-node 0 (- level 1))))))
      (if (path:empty? stck)
          no-current
          (backtrack 0)))
 
    (define (set-current-to-first! tree)
      (define ntyp (node-type tree))
      (define stck (path tree))
      (define (build-path bptr)
        (define node (node:read ntyp bptr))
        (cond ((node:leaf? node)
               (path:push! stck node 1)
               done)
              (else
               (path:push! stck node 0)
               (build-path (node:pointer node 0)))))
      (path:clear! stck)  
      (if (fs:null-block? (tree-root tree))
          no-current
          (build-path (tree-root tree))))
 
    (define (peek tree) 
      (define stck (path tree))
      (if (path:empty? stck)
          no-current
          (let ((node (path:node stck))
                (slot (path:slot stck)))
            (cons (node:key node slot) (node:pointer node slot)))))
 
    (define (update! tree rcid) 
      (define stck (path tree))
      (if (path:empty? stck)
          no-current
          (let ((node (path:node stck))
                (slot (path:slot stck)))
            (node:pointer! node slot rcid)
            (node:write! node)
            done)))
 
    (define (delete! tree) 
      (define ntyp (node-type tree))
      (define stck (path tree))
      (define (traverse-path node)
        (if (path:empty? stck)
            (cond ((= (node:size node) 0)
                   (tree-root! tree (node:pointer node 0))
                   (node:delete! node))
                  (else
                   (node:write! node)))
            (let* ((prnt-node (path:node stck))
                   (prnt-slot (path:slot stck))
                   (left-sibl (and (< 0 prnt-slot)
                                   (node:read ntyp
                                              (node:pointer prnt-node (- prnt-slot 1)))))
                   (push-lkey (and left-sibl
                                   (node:borrow-from-left? 
                                    left-sibl node (node:key prnt-node prnt-slot))))
                   (rght-sibl (and (not push-lkey)
                                   (< prnt-slot (node:size prnt-node))
                                   (node:read ntyp
                                              (node:pointer prnt-node (+ prnt-slot 1)))))
                   (push-rkey (and rght-sibl
                                   (node:borrow-from-right? 
                                    node rght-sibl (node:key prnt-node (+ prnt-slot 1))))))
              (path:pop! stck)
              (cond ((or push-lkey push-rkey) ; we managed to borrow a slot => finish up
                     (cond (push-lkey 
                            (node:write! left-sibl) 
                            (node:key! prnt-node prnt-slot push-lkey))
                           (else
                            (node:write! rght-sibl)
                            (node:key! prnt-node (+ prnt-slot 1) push-rkey)))
                     (node:write! node)
                     (node:write! prnt-node))
                    (else                     ; no borrowing from siblings => merge + recurse
                     (cond (left-sibl
                            (node:merge! 
                             left-sibl node (node:key prnt-node prnt-slot))
                            (node:write! left-sibl)
                            (node:delete! node)
                            (do-delete-key-pointer! prnt-node prnt-slot))
                           (rght-sibl
                            (node:merge!
                             node rght-sibl (node:key prnt-node (+ prnt-slot 1)))
                            (node:write! node)
                            (node:delete! rght-sibl)
                            (do-delete-key-pointer! prnt-node (+ prnt-slot 1)))))))))
      (define (do-delete-key-pointer! node slot)
        (node:key-pointer-delete! node slot)
        (if (< (node:size node) (quotient (node:capacity node) 2))
            (traverse-path node)
            (node:write! node)))
      (if (path:empty? stck)
          no-current
          (let ((node (path:node stck))
                (slot (path:slot stck)))
            (path:pop! stck)
            (do-delete-key-pointer! node slot)
            (path:clear! stck)  
            done)))
 
    (define (flush! tree)
      (disk:write-block! (header tree)))
 
    (define (drop! tree)
      (define ntyp (node-type tree))
      (define root (tree-root tree))
      (define (rec-delete bptr)
        (define node (node:read ntyp bptr))
        (define head (node:pointer node 0))
        (cond ((node:leaf? node)
               (node:delete! node))
              (else
               (rec-delete head)
               (do ((slot 1 (+ slot 1)))
                 ((fs:null-block? (node:pointer node slot))
                  (node:delete! node))
                 (rec-delete (node:pointer node slot))))))
      (if (not (fs:null-block? (tree-root tree)))
          (rec-delete (tree-root tree)))
      (fs:delete-block (header tree))
      (fs:rm (ntype:disk ntyp) (name tree)))
 
    (define (print tree)
      (define ntyp (node-type tree))
      (define disk (ntype:disk ntyp))
      (define bloc (fs:whereis disk (name tree)))
      (define ktyp (ntype:key-type ntyp))
      (define ksiz (ntype:key-size ntyp))
      (define sent (ntype:key-sent ntyp))
      (define root (tree-root tree))

      (define eqls (vector-ref equals ktyp))

   
      (define (print-admin)
        (display "block[")(display bloc)(display "] ADMIN: key-size= ")(display ksiz)
        (display "  key-type= ")(display ktyp)
        (display "  root    = ")(display root)(newline))
   
      (define (traverse-file)
     
        (define (traverse-node node)
          (define pointer0 (node:pointer node 0))
          (define slots (node:capacity node))
          (display "SIZE=")(display (node:size node))
          (display " p0=") (display pointer0) 
          (do ((slot 1 (+ slot 1)))
            ((> slot slots))
            (let ((key (node:key node slot))
                  (pointer (node:pointer node slot)))
              (display " k") (display slot) 
              (display "=") 
              (if (eqls key sent)
                  (display #\?)
                  (display key))
              (display " p") (display slot) 
              (display "=") 
              (display pointer)))
          (newline))
     
        (define (print-rec bptr)
          (define node (node:read ntyp bptr))
          (define head (node:pointer node 0))
          (newline)
          (display "node[")
          (display (node:position node))
          (display "]   ")
          (cond ((fs:null-block? head) ; leaf?
                 (traverse-node node))
                (else
                 (traverse-node node)
                 (print-rec head)
                 (do ((idx 1 (+ idx 1)))
                   ((fs:null-block? (node:pointer node idx)))
                   (print-rec (node:pointer node idx))))))
        (if (not (fs:null-block? root))
            (print-rec root)))
      (print-admin)
      (traverse-file)
      (newline))))