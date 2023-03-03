#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                        DPLL SAT Solver (naive)                  *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dpll-naive)
  (export dpll)
  (import (scheme base)
          (a-d scheme-tools)
          (a-d sat cnf)
          (a-d sat interpretation)
          (a-d sat logger))
  (begin

    (define (conflict? formula interp)
      (exists (lambda (clause)
                (for-all (lambda (lit)
                           (false? interp lit))
                  (literals clause)))
              (clauses formula)))
              
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
 
    ; DPLL algorithm WITHOUT pure literal rule
    (define (dpll formula)
      (define (dpll-rec interpret)
        (log-info "Current interpretation before unit prop:" (interpret->string interpret))
        (unit-prop-naive! formula interpret)
        (log-info "Current interpretation after unit prop:" (interpret->string interpret))
        (cond ((conflict? formula interpret)
               (log-info "A conflicting clause was found.")
               #f)
              ((all-assigned? interpret)
               (log-info "Interpretation fully assigned, we have a solution!")
               interpret)
              (else (let ((x (find-unassigned-atom interpret)))
                      (log-debug "Expanding interpretation...")
                      (or (dpll-rec (true! (from-interpret interpret) x))
                          (dpll-rec (true! (from-interpret interpret) (negate x))))))))
      (dpll-rec (new-interpret formula)))))
