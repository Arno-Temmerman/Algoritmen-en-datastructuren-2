#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*             Conjuctive Normal Form (CNF) formula                *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (cnf)
  (export make-literal literal? variable negation? same-literal? same-var? negate
          make-clause clause? empty-clause? literals clause-size add-clause-in-form!
          make-formula formula? clauses number-of-vars number-of-clauses
          literal->string clause->string formula->string)
  (import (scheme base))
  (begin

    ; Some basic ADTs representing a propositional formula in Conjuctive Normal Form (CNF) from scratch

    ; Since a literal is either a regular atom (variable) or its negation, we define a record type with two fields.
    ; The first field represents the variable of the literal,
    ; while the second field determines whether the literal is a negation or not.
    (define-record-type literal
      (make-literal v n)
      literal?
      (v variable)
      (n negation?))

    (define (literal->string lit)
      (string-append (if (negation? lit)
                         "!x_"
                         "x_")
                     (number->string (variable lit))))
 
    (define same-var? =)
    (define same-neg? equal?)

    (define (same-literal? l1 l2)
      (and (same-var? (variable l1)
                      (variable l2))
           (same-neg? (negation? l1)
                      (negation? l2))))

    (define (negate lit)
      (make-literal (variable lit)
                    (not (negation? lit))))

    ; Clause ADT
    (define-record-type clause
      (make-clause ls s)
      clause?
      (ls literals)  ; list of literals
      (s clause-size))

    (define (empty-clause? cls)
      (and (null? (literals cls))
           (= (clause-size cls) 0)))

    (define (clause->string cls)
      (if (empty-clause? cls)
          "()"
          (string-append "("
                         (let loop
                           ((lits (literals cls)))
                           (if (null? (cdr lits))
                               (literal->string (car lits))
                               (string-append (literal->string (car lits))
                                              " OR "
                                              (loop (cdr lits)))))
                         ")")))
 
    ; Formula ADT
    (define-record-type formula
      (make-formula c nv nc)
      formula?
      (c clauses clauses!)
      (nv number-of-vars)
      (nc number-of-clauses number-of-clauses!))

    (define (formula->string f)
      (let loop
        ((cls (clauses f)))
        (if (null? (cdr cls))
            (clause->string (car cls))
            (string-append (clause->string (car cls))
                           " AND\n "
                           (loop (cdr cls))))))

    (define (add-clause-in-form! f cls)
      (clauses! f (cons cls (clauses f)))
      (number-of-clauses! f (+ (number-of-clauses f) 1)))

    

    ))
