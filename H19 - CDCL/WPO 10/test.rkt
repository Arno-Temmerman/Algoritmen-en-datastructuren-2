#lang r7rs
(import (scheme base)
        (a-d sat cnf)
        (a-d sat interpretation)
        (a-d sat dimacs-parser)
        (a-d sat logger)
        (prefix (a-d sat cdcl standard) standard:)
        ;(a-d sat cdcl alternative)  ;;; UNCOMMENT THIS LINE ONCE DONE
        )

(define test-file "cnf-samples/example-19_18.cnf")

(log-set-verbosity! 'info)
(define formula (parse-dimacs-file test-file))

(log-debug "Checking satisfiability for formula:\n" (formula->string formula))

(define res (standard:cdcl formula))
(log-warning "The standard CDCL result is:" (if res (interpret->string res) res) "\n")
(if res
    (log-info "Is the standard result valid?:" (formula-value formula res) "\n"))

;;; UNCOMMENT BELOW TO TEST YOUR SOLUTION

;(log-info "=======Resetting formula========")
;(set! formula (parse-dimacs-file test-file))
;
;(define res2 (cdcl formula))
;(log-warning "The alternative CDCL result is:" (if res2 (interpret->string res2) res2))
;(if res2
;    (log-info "Is the alternative result valid?:" (formula-value formula res2)))