#lang r7rs
(import (scheme base)
        (a-d sat cnf)
        (a-d sat interpretation)
        (a-d sat dimacs-parser)
        (a-d sat dpll naive iterative)
        (a-d sat logger))

(log-set-verbosity! 'info)
(define formula (parse-dimacs-file "cnf-samples/satisfiable/queens/4queens.cnf"))
(log-debug "Checking satisfiability for formula:\n" (formula->string formula))
(define res (dpll formula))
(log-warning "The DPLL result is:" (if res (interpret->string res) res))
(if res
    (log-warning "Is the result valid?:" (formula-value formula res)))