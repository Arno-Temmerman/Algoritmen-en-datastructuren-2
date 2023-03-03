#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                            Database                             *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                   2010  Software Languages Lab                  *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


(define-library (database)
  (export new open delete!
          create-table drop-table!
          select-from/eq
          insert-into-table! create-index!
          delete-where! print-table print)
  (import (a-d file constants)
          (prefix (a-d disk file-system) fs:)
          (prefix (a-d db table fixed-size-slots table) tbl:)
          (prefix (a-d db table fixed-size-slots schema) scma:)
          (prefix (a-d db index b-tree b-tree) btree:)
          (scheme base)
          (only (scheme write) display))
  (begin
 
    (define *num* 0)
    (define (gennum)
      (let ((res *num*))
        (set! *num* (+ *num* 1))
        res))
 
    (define meta-schema:table `((string  ,fs:filename-size)   ; table name
                                (natural 2)))                 ; table id (foreign key to index table)
    (define table:table-name    0)
    (define table:table-id      1)
 
 
    (define meta-schema:indexes `((natural 2)                 ; table identity
                                  (string  ,fs:filename-size) ; index name
                                  (natural 2)))               ; attribute-number of this index (i.e. in the table)
    (define indexes:tble-idty  0)
    (define indexes:index-name 1)
    (define indexes:key-att    2) 
 
    (define-record-type database
      (make t i)
      database?
      (t tables)
      (i indexes))
 
    (define (new disk name)
      (define tbls (tbl:new disk (string-append "TBL" name) meta-schema:table))
      (define idxs (tbl:new disk (string-append "IDX" name) meta-schema:indexes))
      (make tbls idxs))
 
    (define (open disk name)
      (define tbls (tbl:open disk (string-append "TBL" name)))
      (define idxs (tbl:open disk (string-append "IDX" name)))
      (make tbls idxs))
 
    (define (create-table dbse name scma)
      (define tbls (tables dbse))
      (define disk (tbl:disk tbls))
      (define tble (tbl:new disk name scma))
      (define idty (gennum))
      (tbl:insert! tbls (list name idty))
      tble)
 
    (define (find-id-in-meta-table dbse tabl)
      (define name (tbl:name tabl))
      (define tbls (tables dbse))
      (tbl:set-current-to-first! tbls)
      (let loop
        ((tuple (tbl:peek tbls)))
        (let ((tble-name (car tuple))
              (tble-idty (cadr tuple)))
          display
          (cond ((string=? tble-name name)
                 tble-idty)
                ((not (eq? (tbl:set-current-to-next! tbls) no-current))
                 (loop (tbl:peek tbls)))
                (else
                 not-found)))))
 
    (define (for-all-tables dbse proc)
      (define tbls (tables dbse))
      (define disk (tbl:disk tbls))
      (when (not (eq? (tbl:set-current-to-first! tbls) no-current))
        (let loop
          ((tuple (tbl:peek tbls)))
          (let ((tabl (tbl:open disk (list-ref tuple table:table-name))))
            (if (and (proc tabl)
                     (not (eq? (tbl:set-current-to-next! tbls) no-current)))
                (loop (tbl:peek tbls)))))))
 
    (define (for-all-indices dbse tble proc)
      (define idxs (indexes dbse))
      (define disk (tbl:disk idxs))
      (define idty (find-id-in-meta-table dbse tble))
      (when (not (eq? (tbl:set-current-to-first! idxs) no-current))
        (let loop
          ((tuple (tbl:peek idxs)))
          (cond ((= (list-ref tuple indexes:tble-idty) idty) ; the index belongs to the tble-indx
                 (let ((indx (btree:open disk (list-ref tuple indexes:index-name))))
                   (if (and (proc indx (list-ref tuple indexes:key-att))
                            (not (eq? (tbl:set-current-to-next! idxs) no-current)))
                       (loop (tbl:peek idxs)))))
                ((not (eq? (tbl:set-current-to-next! idxs) no-current))
                 (loop (tbl:peek idxs)))))))
 
    (define (for-all-tuples table proc)
      (if (not (eq? (tbl:set-current-to-first! table) no-current))
          (let loop
            ((tuple (tbl:peek table)))
            (let ((curr (tbl:current table)))
              (if (and (proc tuple curr)
                       (not (eq? (tbl:set-current-to-next! table) no-current)))
                  (loop (tbl:peek table)))))))

    (define (create-index! dbse tabl name attribute) 
      (define disk (tbl:disk tabl))
      (define tbls (tables dbse))
      (define idxs (indexes dbse))
      (define idty (find-id-in-meta-table dbse tabl))
      (define scma (tbl:schema tabl))
      (define indx (btree:new disk name 
                              (scma:type scma attribute) 
                              (scma:size scma attribute)))
      (tbl:insert! idxs (list idty name attribute)) 
      (for-all-tuples
       tabl
       (lambda (tuple rid)
         (btree:insert! indx (list-ref tuple attribute) rid)))
      (tbl:close! idxs)
      (btree:flush! indx))
 
    (define (insert-into-table! dbse tble tuple)
      (define rcid  (tbl:insert! tble tuple))
      (tbl:close! tble)
      (for-all-indices dbse tble 
                       (lambda (indx att)
                         (btree:insert! indx (list-ref tuple att) rcid)
                         (btree:flush! indx))))
 
    (define (select-from/eq dbse tble attr valu)
      (define scma (tbl:schema tble))
      (define type (scma:type scma attr))
      (define eqls (vector-ref equals type))           ;right equality procedure
      (define indx '())
      (define rslt '())
      (for-all-indices dbse tble (lambda (idx att)     ;first try to find an index on 'attr'
                                   (when (= att attr)
                                     (set! indx idx)
                                     #f)))
      (if (null? indx)        ; index on 'attr' found, or search the tuple file sequentially
          (for-all-tuples tble (lambda (tple rcid)
                                 (if (eqls (list-ref tple attr) valu)
                                     (set! rslt (cons (tbl:peek tble) rslt)))))
          (for-all-identical-keys indx eqls valu
                                  (lambda (rcid)
                                    (tbl:current! tble (cdr (btree:peek indx)))
                                    (set! rslt (cons (tbl:peek tble) rslt)))))
      rslt)

    (define (for-all-identical-keys indx eqls valu proc)
      (let loop
        ((cur? (eq? (btree:find! indx valu) done)))
        (if cur?
            (loop (and (proc (cdr (btree:peek indx)))
                       (eq? (btree:set-current-to-next! indx) done)
                       (eqls (car (btree:peek indx)) valu))))))
 
    (define (delete-from-indexes dbse tble eqls tple rcid)
      (for-all-indices
       dbse tble
       (lambda (indx att)
         (for-all-identical-keys indx eqls (list-ref tple att)
                                 (lambda (rcid2) ; but only if it is THIS tuple?
                                   (when (equal? rcid2 rcid)
                                     (btree:delete! indx)
                                     #f)))
         (btree:flush! indx))))

    (define (find-tuple-rcid dbse tble eqls attr valu)
      (define indx '())
      (define rcid '())
      (for-all-indices dbse tble (lambda (idx att) ;first try to find an index on 'attr'
                                   (when (= att attr)
                                     (set! indx idx)
                                     #f)))
      (cond ((not (null? indx)) ; exists index leading to the tuple
             (when (eq? (btree:find! indx valu) done)
               (set! rcid (cdr (btree:peek indx)))
               (tbl:current! tble rcid)))
            (else               ; there is no index => search tuple sequentially in the table
             (for-all-tuples tble (lambda (tple rid2)
                                    (when (eqls (list-ref tple attr) valu)
                                      (set! rcid rid2)
                                      #f)))))
      rcid)
  
    (define (delete-where! dbse tble attr valu)
      (define scma (tbl:schema tble))
      (define type (scma:type scma attr))
      (define eqls (vector-ref equals type))
      (let loop
        ((rcid (find-tuple-rcid dbse tble eqls attr valu)))
        (unless (null? rcid)
          (let ((tple (tbl:peek tble)))
            (tbl:delete!  tble rcid)
            (tbl:close! tble)
            (delete-from-indexes dbse tble eqls tple rcid))
          (loop (find-tuple-rcid dbse tble eqls attr valu)))))
 
    (define (delete-from-meta-table dbse tabl)
      (define name (tbl:name tabl))
      (define tbls (tables dbse))
      (tbl:set-current-to-first! tbls)
      (let find-table
        ((tuple (tbl:peek tbls)))
        (let ((tble-name (car tuple)))
          (cond ((string=? tble-name name)
                 (tbl:delete! tbls (tbl:current tbls)))
                ((not (eq? (tbl:set-current-to-next! tbls) no-current))
                 (find-table (tbl:peek tbls)))
                (else not-found)))))
 
    (define (drop-table! dbse table)
      (define tbls (tables dbse))
      (define idxs (indexes dbse))
      (define disk (tbl:disk tbls))
      (for-all-indices dbse table 
                       (lambda (indx att)
                         (btree:drop! indx)))
      (delete-from-meta-table dbse table)
      (tbl:drop! table))
 
    (define (delete! dbse)
      (define tbls (tables dbse))
      (define idxs (indexes dbse))
      (for-all-tables dbse
                      (lambda (table)
                        (drop-table! dbse table)))
      (tbl:drop! tbls)
      (tbl:drop! idxs))
 
    (define (print dbse)
      (define tbls (tables dbse))
      (define idxs (indexes dbse))
      (display "DATABASE   METATABLE of TABLES")(newline)
      (display "           ===================")(newline)
      (tbl:print tbls)
      (display "           METATABLE of INDEXES")(newline)
      (display "           ===================")(newline)
      (tbl:print idxs))
    (define (print-table db tble)
      (tbl:print tble))))