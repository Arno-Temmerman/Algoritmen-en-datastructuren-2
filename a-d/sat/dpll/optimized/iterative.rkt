#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*       DPLL SAT Solver (with Two-Watched Literal Scheme)         *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (dpll-twls)
  (export dpll)
  (import (scheme base)
          (scheme write)
          (a-d sat cnf)
          (a-d sat logger)
          (a-d sat interpretation)
          (prefix (a-d sat twls) twls:)
          (prefix (a-d stack linked) stack:)
          (prefix (a-d queue linked) queue:))
  (begin

    ; DPLL algorithm with explicit stack (iterative tree search)
    (define (dpll formula)
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
        (queue:enqueue! prop-q lit))

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
                                 (cond (lit 
                                        ; watch(er) van ¬l en l' switchen
                                        (log-debug "Found unwatched non-false literal:" (literal->string lit))
                                        (twls:remove-clause-from-var-watch! twls not-l watched-cls)
                                        (twls:add-clause-to-var-watch!      twls lit   watched-cls)
                                        (twls:change-watch! watched-cls not-l lit))
                                       (else
                                        (log-debug "No other non-false literal in clause...")
                                        ; andere watched literal checken
                                        (set! lit (twls:other-watch watched-cls not-l))
                                        (log-debug "Checking the other watch " (literal->string lit))
                                        (cond ((unknown? interpret lit)
                                               (log-debug "The other watch is unknown: we make it true.")
                                               (make-true! lit interpret (twls:clause watched-cls)))
                                              ((false? interpret lit) ; => clause false => CNF false
                                               (log-debug "CONFLICTING CLAUSE!")
                                               (clear-queue!) ;clear
                                               (return (twls:clause watched-cls))))))
                                 ))             
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

      (define (dpll-iter)
        (if (not (stack:empty? stack))
            (let ((interpret (stack:pop! stack)))
              (if (not (null? (latest-assigned-lit interpret)))
                  (queue:enqueue! prop-q (latest-assigned-lit interpret)))
              (log-info "Current interpretation before unit prop:" (interpret->string interpret))
              (let ((conflict? (unit-prop! interpret)))
                (log-info "Current interpretation after unit prop:" (interpret->string interpret))
                (cond (conflict?
                       (log-info "A conflicting clause was found:" (clause->string conflict?))
                       (dpll-iter))
                      ((all-assigned? interpret)
                       (log-info "Interpretation fully assigned, we have a solution!")
                       interpret)
                      (else (let ((x (find-unassigned-atom interpret)))
                              (when x
                                (log-debug "Expanding interpretation...")
                                (stack:push! stack (true! (from-interpret interpret) (negate x)))
                                (stack:push! stack (true! (from-interpret interpret) x)))
                              (dpll-iter))))))
            #f))
      (call-with-current-continuation
       (lambda (cont)
         (set! exit cont)
         (stack:push! stack (propagate-single-lit-clauses! (new-interpret formula)))
         (dpll-iter))))
    ))