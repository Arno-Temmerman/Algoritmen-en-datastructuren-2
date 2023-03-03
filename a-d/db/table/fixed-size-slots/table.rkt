#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                         Relational Tables                       *-*-
;-*-*                                                                 *-*-
;-*-*                        Wolfgang De Meuter                       *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (table)
  (export new open close! current current!
          drop! insert! delete! peek print
          set-current-to-first! set-current-to-next!
          schema name disk)
  (import (a-d file constants)
          (prefix (a-d disk config) disk:)
          (prefix (a-d disk file-system) fs:)
          (prefix (a-d db rcid) rcid:)
          (prefix (a-d db table fixed-size-slots schema) scma:)
          (prefix (a-d db table fixed-size-slots node) node:)
          (scheme base)
          (scheme write))
  (begin
 
    (define-record-type table
      (make n h s b l)
      table?
      (n name)
      (h header header!)
      (s schema schema!)
      (b buffer buffer!)
      (l slot   slot!))
 
    (define full-offset   0)
    (define last-offset   (+ full-offset disk:block-ptr-size))
    (define part-offset   (+ last-offset disk:block-ptr-size))
    (define schema-offset (+ part-offset disk:block-ptr-size))
 
    (define (full tble)
      (define hder (header tble))
      (disk:decode-fixed-natural hder full-offset disk:block-ptr-size))
    (define (full!  tble bptr)
      (define hder (header tble))
      (disk:encode-fixed-natural! hder full-offset disk:block-ptr-size bptr))
    (define (last tble)
      (define hder (header tble))
      (disk:decode-fixed-natural hder last-offset disk:block-ptr-size))
    (define (last!  tble bptr)
      (define hder (header tble))
      (disk:encode-fixed-natural! hder last-offset disk:block-ptr-size bptr))
    (define (part tble)
      (define hder (header tble))
      (disk:decode-fixed-natural hder part-offset disk:block-ptr-size))
    (define (part! tble bptr)
      (define hder (header tble))
      (disk:encode-fixed-natural! hder part-offset disk:block-ptr-size bptr))
    (define (schema-bptr tble)
      (define hder (header tble))
      (disk:decode-fixed-natural hder schema-offset disk:block-ptr-size))
    (define (schema-bptr! tble bptr)
      (define hder (header tble))
      (disk:encode-fixed-natural! hder schema-offset disk:block-ptr-size bptr))
 
    (define (disk tble)
      (disk:disk (header tble)))
 
    (define (new disk name atts)
      (define scma (scma:new disk atts))
      (define hder (fs:new-block disk))
      (define tble (make name hder scma '() -1))
      (full! tble fs:null-block)
      (last! tble fs:null-block)
      (part! tble fs:null-block)
      (schema-bptr! tble (scma:position scma))
      (disk:write-block! hder)
      (fs:mk disk name (disk:position hder))
      tble)
 
    (define (open disk name)
      (define hptr (fs:whereis disk name))
      (define hder (disk:read-block disk hptr))
      (define tble (make name hder '() '() -1))
      (define sptr (schema-bptr tble))
      (define scma (scma:open disk sptr))
      (schema! tble scma)
      tble)
 
    (define (close! tble)
      (disk:write-block! (header tble)))
 
    (define (peek tble)
      (define bffr (buffer tble))
      (define curr (slot tble))
      (if (null? bffr)
          no-current
          (node:record bffr curr)))
 
    (define (extract-node! tble first first! node) ; This procedure is not as clean as I hoped it to be (c.f. when/if/if)
      (define next-bptr (node:next node))
      (define prev-bptr (node:previous node))
      (node:next!     node fs:null-block)
      (node:previous! node fs:null-block)
      (if (not (fs:null-block? next-bptr))
          (let ((next (node:read (schema tble) next-bptr)))
            (node:previous! next prev-bptr)
            (node:write! next)))
      (if (not (fs:null-block? prev-bptr))
          (let ((prev (node:read (schema tble) prev-bptr)))
            (node:next! prev next-bptr)
            (node:write! prev)))
      (when (= (first tble) (last tble) (node:position node)) ; delete only node of full-list
        (first! tble fs:null-block)
        (last!  tble fs:null-block))
      (if (= (first tble) (node:position node)) ; prev | frst -> next
          (first! tble next-bptr))
      (if (= (last tble) (node:position node))
          (last! tble prev-bptr)))

    (define (insert-node! tble first first! node)
      (define frst-bptr (first tble))
      (if (not (fs:null-block? frst-bptr))
          (let ((next (node:read (schema tble) frst-bptr)))
            (node:previous! next (node:position node))
            (node:next! node frst-bptr)
            (node:write! next)))
      (first! tble (node:position node)))

    (define (insert-part! tble node)
      (define last-bptr (last tble))
      (insert-node! tble part part! node)
      (if (not (fs:null-block? last-bptr)) ; there are nodes in full-list
          (let ((prev (node:read (schema tble) last-bptr)))
            (node:next! prev (node:position node))
            (node:previous! node last-bptr)
            (node:write! prev))))
 
    (define (insert-full! tble node)
      (define last-bptr (last tble))
      (define part-bptr (part tble))
      (insert-node! tble full full! node)
      (when (fs:null-block? last-bptr) ; first node to be inserted in full-list
        (last! tble (node:position node))
        (if (not (fs:null-block? part-bptr)) ; there are still nodes in part-list
            (let ((next (node:read (schema tble) part-bptr)))   
              (node:next! node (node:position next))
              (node:previous! next (node:position node))
              (node:write! next)))))
 
    (define (insert! tble tupl)
      (define scma (schema tble))
      (define room (part tble))
      (define node (if (fs:null-block? room)
                       (let ((new (node:new scma fs:null-block)))
                         (insert-part! tble new)
                         new)
                       (node:read scma room)))
      (define free (find-free-slot node -1))
      (node:record! node free tupl)
      (when (node:all-occupied? node)
        (extract-node! tble part part! node)
        (insert-full!  tble node))
      (node:write!  node)
      (buffer!      tble node)
      (slot!        tble free)
      (rcid:new (node:position node) free))

    (define (delete! tble rcid)
      (define scma (schema tble))
      (define node (node:read scma (rcid:bptr rcid)))
      (define was-full? (node:all-occupied? node))
      (node:clear-slot! node (rcid:slot rcid))
      (cond ((node:all-free? node)
             (if was-full?
                 (extract-node! tble full full! node)
                 (extract-node! tble part part! node))
             (node:delete! node))
            (else
             (when was-full?
               (extract-node! tble full full! node)
               (insert-part! tble node))
             (node:write! node)))
      (buffer! tble '())
      (slot!   tble -1))
 
    (define (drop! tble)
      (define scma (schema tble))
      (define hder (header tble))
      (define disk (scma:disk scma))
      (define (delete-nodes bptr)
        (if (not (fs:null-block? bptr))
            (let*
                ((node (node:read scma bptr))
                 (next (node:next node)))
              (node:delete! node)
              (delete-nodes next))))
      (if (fs:null-block? (full tble))
          (delete-nodes (part tble))
          (delete-nodes (full tble)))
      (scma:delete! scma)
      (fs:delete-block hder)
      (fs:rm disk (name tble)))
 
    (define (print tble)
      (define scma (schema tble))
      (define hder (header tble))
      (define (print-node node idx)
        (cond ((= idx (scma:capacity scma))
               )
              ((node:slot-occupied? node idx)
               (display "    |")
               (display (node:record node idx)) (newline)
               (print-node node (+ idx 1)))
              (else
               (display "    |<< empty slot >>")(newline)
               (print-node node (+ idx 1)))))
      (display (list "    TABLE " (name tble)))(newline)
      (display (list (string-append "    ------ " (make-string (string-length (name tble)) #\-))))(newline)
      (if (not (and (fs:null-block? (full tble))
                    (fs:null-block? (part tble))))
          (let ((fptr (if (fs:null-block? (full tble))
                          (part tble)
                          (full tble))))
            (let loop
              ((curr (node:read scma fptr)))
              (let ((next (node:next curr)))
                (display (list "    : node" (node:position curr)))(newline)
                (display (list "    : prev" (node:previous curr) "next" (node:next curr) ))(newline)
                (print-node curr 0)
                (when (not (fs:null-block? next))
                  (display "    -")(newline)
                  (loop (node:read scma next))))))))
 
    (define (find-occupied-slot node strt)
      (define scma (node:schema node))
      (let loop
        ((cntr (+ strt 1)))
        (cond ((= (scma:capacity scma) cntr)
               -1)
              ((node:slot-occupied? node cntr)
               cntr)
              (else
               (loop (+ cntr 1))))))
 
    (define (find-free-slot node strt)
      (define scma (node:schema node))
      (let loop
        ((cntr (+ strt 1)))
        (cond ((= (scma:capacity scma) cntr)
               -1)
              ((node:slot-occupied? node cntr)
               (loop (+ cntr 1)))
              (else
               cntr))))
 
    (define (set-current-to-first! tble)
      (define scma (schema tble))
      (if (and (fs:null-block? (full tble))
               (fs:null-block? (part tble)))
          no-current
          (let* ((fptr (if (fs:null-block? (full tble))
                           (part tble)
                           (full tble)))
                 (bffr (node:read scma fptr))
                 (curr (find-occupied-slot bffr -1)))
            (buffer! tble bffr)
            (slot!   tble curr)
            done)))
 
    (define (set-current-to-next! tble)
      (define scma (schema tble))
      (define bffr (buffer tble))
      (define curr (slot   tble))
      (if (null? bffr)
          no-current
          (let ((indx (find-occupied-slot bffr curr)))
            (cond ((not (= indx -1))
                   (buffer! tble bffr)
                   (slot!   tble indx)
                   done)
                  ((not (fs:null-block? (node:next bffr)))
                   (let* ((next (node:read scma (node:next bffr)))
                          (indx  (find-occupied-slot next -1)))
                     (buffer! tble next)
                     (slot!   tble indx)
                     done))
                  (else
                   (buffer! tble '())
                   (slot!   tble -1)
                   no-current)))))
 
    (define (current tble)
      (define bffr (buffer tble))
      (define curr (slot   tble))
      (if (null? bffr)
          no-current
          (rcid:new (node:position bffr) curr)))
 
    (define (current! tble rcid)
      (define bffr (buffer tble))
      (define curr (rcid:slot rcid))
      (if (or (null? bffr)
              (not (= (rcid:bptr rcid) (node:position bffr))))
          (set! bffr (node:read (schema tble) (rcid:bptr rcid))))
      (buffer! tble bffr)
      (slot!   tble curr))))