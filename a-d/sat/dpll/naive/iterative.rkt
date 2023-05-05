#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*          DPLL SAT Solver (pure literal elimination)             *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dpll-pure-literal)
  (export dpll)
  (import (scheme base)
          (a-d scheme-tools)
          (a-d sat cnf)
          (a-d sat logger)
          (a-d sat interpretation)
          (prefix (a-d stack linked) stack:))
  (begin

    (define (conflict? formula interp)
      (exists (lambda (clause)
                (for-all (lambda (lit)
                           (false? interp lit))
                  (literals clause)))
              (clauses formula)))

    (define (find-pure-literals formula interpret)
      (define num-lits (number-of-vars formula))
      (define lits '())
      (define (find-clauses-containing l)
        (filter (lambda (clause)
                  (and (eq? (clause-value clause interpret) unknown)
                       (memp (lambda (lit)
                               (same-literal? lit l))
                             (literals clause))))
                (clauses formula)))
      ; for-each possible literal in the formula
      (let iter-literals
        ((current-var 1))
        (log-debug "Current var:" current-var)
        (let ((pos-clauses (find-clauses-containing (make-literal current-var #f)))
              (neg-clauses (find-clauses-containing (make-literal current-var #t))))
          (log-debug "Positive clauses:" (map clause->string pos-clauses))
          (log-debug "Negative clauses:" (map clause->string neg-clauses))
          (cond ((and (null? pos-clauses)
                      (not (null? neg-clauses))
                      (unknown? interpret (make-literal current-var #t)))
                 (set! lits (cons (make-literal current-var #t) lits))) ; we must make !l true
                ((and (null? neg-clauses)
                      (not (null? pos-clauses))
                      (unknown? interpret (make-literal current-var #f)))         
                 (set! lits (cons (make-literal current-var #f) lits)))) ; we must make l true
          (if (< current-var num-lits)
              (iter-literals (+ current-var 1)))))
      (log-debug "Found pure literals: " (map literal->string lits))
      lits)

    (define (pure-lits! formula interpret)
      (define changed? #f)
      (for-each (lambda (lit)
                  (log-debug "Assigning pure literal: " (literal->string lit))
                  (true! interpret lit)
                  (set! changed? #t))
                (find-pure-literals formula interpret))
      (if changed?
          (pure-lits! formula interpret)
          interpret))
        
    (define (find-unit clause interp)
      (define cls-siz (clause-size clause))
      (define count-false 0)
      (define latest-literal #f)
      (log-debug "Checking if a clause is unit...")
      (for-each (lambda (literal)
                  (if (false? interp literal)
                      (set! count-false (+ count-false 1)) ; we count the actual occurences of "false"
                      (set! latest-literal literal))) ; we remember the literal not being false
                (literals clause))
      (log-debug "Clause checked.")
      (if (and (= count-false (- cls-siz 1))
               (unknown? interp latest-literal)) ; verify if the clause was not already fully assigned
          latest-literal ; this is the only literal not being assigned yet in the clause
          #f))

    (define (unit-prop-naive! formula interpret)
      (define changed? #f)
      ; loop over the clauses
      (log-debug "Progating units...")
      (for-each (lambda (curr-clause)
                  (let ((unit (find-unit curr-clause interpret)))
                    (when unit
                      (log-debug "Found unit clause:" (literal->string unit))
                      (true! interpret unit)
                      (log-debug "Atom value assigned in interpretation.")
                      (set! changed? #t))))
                (clauses formula))
      (log-debug "Progating units done.")
      (if changed? ; until fixpoint is reached...
          (unit-prop-naive! formula interpret)
          interpret))
 
    ; DPLL algorithm WITH pure literal rule
    (define (dpll formula)
      (define s (stack:new))
      
      (define (dpll-iter)
        (if (stack:empty? s)
            #f
            (let ((interpret (stack:pop! s)))
        (log-info "Current interpretation before unit prop:" (interpret->string interpret))
        (unit-prop-naive! formula interpret)
        (log-info "Current interpretation after unit prop:" (interpret->string interpret))
        (log-debug "Eliminating pure literals...")
        (pure-lits! formula interpret)
        (log-debug "Eliminating pure literals done.")
        (log-info "Current interpretation after pure lit elim:" (interpret->string  interpret))

        (cond ((conflict? formula interpret)
               (log-info "A conflicting clause was found.")
               (dpll-iter))
              ((all-assigned? interpret)
               (log-info "Interpretation fully assigned, we have a solution!")
               interpret)
              (else (let ((x (find-unassigned-atom interpret)))
                      (log-debug "Expanding interpretation...")
                            (stack:push! s (true! (from-interpret interpret) x))
                            (stack:push! s (true! (from-interpret interpret) (negate x)))
                            (dpll-iter)))))))
      
      (stack:push! s (new-interpret formula))
      (dpll-iter))))