#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                          CDCL SAT Solver                        *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2023 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (cdcl)
  (export cdcl)
  (import (scheme base)
          (a-d scheme-tools)
          (a-d sat cnf)
          (a-d sat logger)
          (a-d sat interpretation)
          (prefix (a-d sat twls) twls:)
          (prefix (a-d stack linked) stack:)
          (prefix (a-d queue linked) queue:)
          (a-d sorting internal comparative quicksort))
  (begin
 
    ;; Creates a new list of unique elements from a given list that might contain duplicate elements.
    (define (remove-duplicates lst ==?)
      (fold-right (lambda (elem acc-lst)
                    (cons elem (filter (lambda (z)
                                         (not (==? elem z)))
                                       acc-lst)))
                  '()
                  lst))

    (define (resolve cls1 cls2 l not-l)
      (log-debug "Resolving" (clause->string cls1) "with" (clause->string cls2))
      (let ((merged-lits (remove-duplicates (append (remp (lambda (lit)
                                                            (same-literal? lit l))
                                                          (literals cls1))
                                                    (remp (lambda (lit)
                                                            (same-literal? lit not-l))
                                                          (literals cls2)))
                                            same-literal?)))
        (make-clause merged-lits (length merged-lits))))

    (define (cdcl formula)
      (define interpret (new-interpret formula))
      (define stack (stack:new))
      (define prop-q (queue:new))
      (define twls (twls:new formula))
      (define current-decision-level 0)
      (define exit '())
   
      (define (make-true! lit interpret reason)
        (log-debug "Making things come TRUE:" (literal->string lit))
        (true! interpret lit)
        (twls:reason! twls lit reason)
        (twls:level! twls lit current-decision-level)
        (twls:time! twls lit (current-time)) ;; NEW: We log a timestamp
        (queue:enqueue! prop-q lit)
        (stack:push! stack lit)) ;;; NEW

      (define (clear-queue!)
        (let loop ()
          (unless (queue:empty? prop-q)
            (queue:serve! prop-q)
            (loop))))

      (define (unit-prop! interpret)
        (call-with-current-continuation
         (lambda (return)
           (let loop ()
             (if (not (queue:empty? prop-q)) ;; while prop-queue not empty
                 (let* ((l (queue:serve! prop-q))
                        (not-l (negate l)))
                   (log-debug "Serving literal from queue:" (literal->string l))
                   (true! interpret l)
                   (log-debug "Made literal" (literal->string l) "true.")
                   (log-debug "Processing watched clauses of:" (literal->string not-l))
                   (log-debug (map (lambda (wcls)
                                     (clause->string (twls:clause wcls)))
                                   (twls:get-lit-watchers twls not-l)))
                   (for-each (lambda (watched-cls)
                               (log-debug "Current clause:" (clause->string (twls:clause watched-cls)))
                               (let ((lit (twls:find-unwatched-nonfalse-lit watched-cls interpret)))
                                 (if lit
                                     (begin
                                       (log-debug "Found unwatched non-false literal:" (literal->string lit))
                                       (twls:remove-clause-from-var-watch! twls not-l watched-cls) 
                                       (twls:add-clause-to-var-watch! twls lit watched-cls)
                                       (twls:change-watch! watched-cls not-l lit))
                                     (let ((other-lit (twls:other-watch watched-cls not-l)))
                                       (log-debug "No other non-false literal in clause...")
                                       (log-debug "Checking the other watch: " (literal->string other-lit))
                                       (cond ((true? interpret other-lit)
                                              (log-debug "The other watch is true.")
                                              #t)
                                             ((unknown? interpret other-lit) ; potential unit
                                              (log-debug "The other watch is unknown: we make it true.")
                                              (make-true! other-lit interpret (twls:clause watched-cls)))
                                             (else ; the other watch is false... we have a conflict!
                                              (log-debug "CONFLICTING CLAUSE!")
                                              (clear-queue!)
                                              (return (twls:clause watched-cls))))))))             
                             (twls:get-lit-watchers twls not-l))
                   (loop))))
           #f)))

      (define (propagate-single-lit-clauses! interpret)
        (for-each (lambda (clause)
                    (if (= (clause-size clause) 1)
                        (let ((lit (car (literals clause))))
                          (log-debug "We have a single lit clause:" (clause->string clause))
                          (cond ((false? interpret lit)
                                 (exit clause)) 
                                ((unknown? interpret lit)
                                 (make-true! lit interpret clause))))))
                  (clauses formula))
        interpret)

      (define (has-two-or-more-lits-from-curr-dec-lvl? cls)
        (define ctr 0)
        (for-each (lambda (lit)
                    (if (= (twls:level twls lit)
                           current-decision-level)
                        (set! ctr (+ ctr 1))))
                  (literals cls))
        (> ctr 1))

      (define (youngest-literal cls)
        (let* ((lits (literals cls))
               (times (map (lambda (lit)
                             (twls:time twls lit))
                           lits))
               (curr-lit (car lits))
               (curr-time (car times)))
          (for-each (lambda (lit time)
                      (when (time<? curr-time time)
                        (set! curr-time time)
                        (set! curr-lit lit)))
                    (cdr lits)
                    (cdr times))
          curr-lit))

      (define (second-youngest-lit cls)
        (if (= (clause-size cls) 1)
            (car (literals cls))
            (let ((elems (list->vector 
                          (map (lambda (lit)
                                 (cons lit
                                       (twls:time twls lit)))
                               (literals cls)))))
              (sort elems (lambda (pair1 pair2)
                            (let ((time1 (cdr pair1))
                                  (time2 (cdr pair2)))
                              (time>? time1 time2))))
              (car (vector-ref elems 1)))))

      (define (analyze-conflict confl-cls)
        (let loop
          ((c-new confl-cls))
        (if (has-two-or-more-lits-from-curr-dec-lvl? c-new)
            (let* ((l (youngest-literal confl-cls))
                   (not-l (negate l))
                   (c-next (if (memp (lambda (lit)
                                       (same-literal? lit
                                                      not-l))
                                     (literals c-new))
                            
                               (resolve c-new
                                        (twls:reason twls l)
                                        not-l
                                        l)
                               c-new)))
              
                (twls:reason! twls l '())
                (unknown! interpret l)
                (twls:level! twls l -1)
                (twls:time! twls l #f)
                (loop c-next))
              c-new)))

      (define (backjump! confl-cls lvl)
        (define (jump-iter)
          (let ((l (stack:top stack)))
            (when (> (twls:level twls l)
                     lvl)
              (log-debug "Popping literal" (literal->string l) "from trail stack.")
              (stack:pop! stack)
              (unknown! interpret l)
              (twls:reason! twls l '())
              (twls:level! twls l -1)
              (twls:time! twls l #f) ;; we forget when we assigned this lit
              (jump-iter))))
        (jump-iter)
        (let ((lit (let loop ((ls (literals confl-cls)))
                     (if (null? ls)
                         (exit #f)
                         (let ((l (car ls)))
                           (if (unknown? interpret l)
                               l
                               (loop (cdr ls))))))))
          (log-debug "Jumped backed until interpret:" (interpret->string interpret))
          (set! current-decision-level lvl)
          (make-true! lit interpret confl-cls)))
         
      (define (add-clause! cls)
        ; add to TWLS
        (twls:add-clause! twls cls)
        ; add to formula
        (add-clause-in-form! formula cls))

      (define (cdcl-iter)
        (log-info "Current level:" current-decision-level)
        (log-info "Current interpretation before unit prop:" (interpret->string interpret))
        (let ((conflicting-clause (unit-prop! interpret)))
          (log-info "Current interpretation after unit prop:" (interpret->string interpret))
          (cond (conflicting-clause
                 (log-info "A conflicting clause was found:" (clause->string conflicting-clause))
                 (if (empty-clause? conflicting-clause)
                     (exit #f))
                 (let ((c-new (analyze-conflict conflicting-clause)))
                   (log-info "Analyzed conflict. New learned clause:"(clause->string c-new))
                   (log-debug "Starting backjump...")
                   (if (= (clause-size c-new) 1)
                       (begin (backjump! c-new 0)
                              (add-clause-in-form! formula c-new)
                              (propagate-single-lit-clauses! interpret)
                              (stack:push! stack (find-unassigned-atom interpret)))
                       (begin  (backjump! c-new (twls:level twls (second-youngest-lit c-new)))
                               (add-clause! c-new)))
                   (log-debug "Backjump done.")
                   (log-debug "Clause added.")))
                ((find-unassigned-atom interpret)
                 (let ((x (find-unassigned-atom interpret)))
                   (log-debug "Increasing dec level.")
                   (set! current-decision-level (+ current-decision-level 1))
                   (make-true! x interpret 'dec)))
                (else (log-info "Interpretation fully assigned, we have a solution!")
                      (exit interpret))))
        (cdcl-iter))
     
      (call-with-current-continuation
       (lambda (cont)
         (set! exit cont)
         (propagate-single-lit-clauses! interpret)
         (stack:push! stack (find-unassigned-atom interpret))
         (cdcl-iter))))
    ))
