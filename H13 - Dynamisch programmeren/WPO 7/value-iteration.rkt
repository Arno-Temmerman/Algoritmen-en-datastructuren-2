#lang r7rs
(import (scheme base)
        (scheme write)
        (a-d scheme-tools)
        (07-dynamisch-programmeren Opgave gridworld-mdp))

; Some testing environments
(define default-discount 1)
(define env1 (make-env 3 3 manh-steps default-discount))
(define env2 (make-env 4 4 manh-steps default-discount))
(define env3 (make-env 3 3 king-steps default-discount))
(define env4 (make-env 4 4 king-steps default-discount))
(define env5 (make-env 5 5 manh-steps default-discount))
(define env6 (make-env 5 5 king-steps default-discount))

; Determine the optimal value function v*(s) by computing v* for each state s in the environment:
; How much reward can we obtain "on average" beginning from a certain state?
; We realize this by creating a higher-order procedure.


;; Attempt 1: Very Naive recursion.
(define (v*-very-naive env)
  
  ; Aux. ADT to keep track of visited cells during the recursion
  (define (make-visited-table)
    (make-2D-vector (num-cols env) (num-rows env) (lambda (i j) #f)))
  (define (visit table state)
    ; in every descent in the recursion we need a new table!
    (make-2D-vector (num-cols env) (num-rows env) (lambda (i j)
                                                    (if (same-state? state (make-2D-cell i j))
                                                        #t
                                                        (ij? table i j)))))
  (define (value* state visited)
    (if (terminal? env state)
        0
        (apply max (map (lambda (action)
                          (let ((next-state (transition env state action)))
                            (if (ij? visited (get-x state) (get-y state)) ; avoid walking in circles
                                -inf.0 ; neutral element for max
                                (+ (reward env state action next-state)
                                   (* (discount env) (value* next-state (visit visited state)))))))
                        (actions env)))))                                                            
  (lambda (state)
    (value* state
            (make-visited-table))))

;; Attempt 2: Top-down + memoized recursion (storing previously computed intermediate results)
(define (v*-naive env)

  (define value-table (make-2D-vector (num-cols env) (num-rows env) (lambda (i j) #f)))
  
  (define (make-visited-table)
    (make-2D-vector (num-cols env) (num-rows env) (lambda (i j) #f)))
  (define (visit table state)
    (let ((new-table (make-2D-vector (num-cols env) (num-rows env) (lambda (i j) (ij? table i j)))))
      (ij! new-table (get-x state) (get-y state) #t)
      new-table))
    
  (define (value* state visited)
    (if (terminal? env state)
        0
        (let ((i  (get-x state))
              (j (get-y state)))
          (if (not (ij? value-table i j))
              (ij! value-table i j (apply max (map (lambda (action)
                                                     (let ((next-state (transition env state action)))
                                                       (if (ij? visited (get-x next-state) (get-y next-state))
                                                           -inf.0 ; avoid walking in cirlcles
                                                           (+ (reward env state action next-state)
                                                              (* (discount env)
                                                                 (value* next-state
                                                                         (visit visited state)))))))
                                                   (actions env)))))
          (ij? value-table i j))))

  (lambda (state)
    (value* state
            (make-visited-table))))

;; Attempt 3: Dynamic Programming: Bottom-up approach
;; Value-iteration

;; YOUR JOB today is to fill in the body of this function :-)

(define (value-iteration threshold)
  (lambda (env)
    (define value-table (make-2D-vector (num-cols env)
                                        (num-rows env)
                                        (lambda (i j) 0)))
    (let iter
      ((delta 0))

      ;;; SOMETHING MISSING HERE... 
 
      (if (< delta threshold)
          (lambda (state)
            (ij? value-table (get-x state) (get-y state)))
          (iter 0)))))

; Policy: Mapping states to actions given the value function
; Return the action leading to a best possible state transition
(define (policy env v*)
  (lambda (state)
    (car (fold-left (lambda (acc action-value)
                      (if (> (cdr action-value)
                             (cdr acc))
                          action-value
                          acc))
                    (cons (make-action 0 0) -inf.0) ; NOOP
                    (map (lambda (move)
                           (let* ((next-state (transition env state move))
                                  (next-value (v* next-state))
                                  (state-value-estim (+ (reward env state move next-state)
                                                        (* (discount env) next-value))))
                             (cons move
                                   state-value-estim)))
                         (actions env))))))

; An agent interactively executes the policy to perform a task
(define (run-policy policy env)
  (let loop
    ((state (start env)))
    (display state)
    (let ((best-action (policy state)))
      (if (terminal? env state)
          (display " DONE\n")
          (begin (display " Action = ")(display (action-name env best-action))(newline)
                (loop (transition env state best-action)))))))

;; TESTING the attempts
(define (test-policies v*-funs envs)
  (for-each (lambda (env)
              (display "Env: ")(display (num-cols env)) (display "c ") (display (num-rows env))(display "r\n")
              (for-each (lambda (name-v*)
                          (display (car name-v*))(newline)
                          (run-policy (policy env ((cdr name-v*) env)) env))
                        v*-funs))
            envs))

;;; All attempts work on small/simple environment
(test-policies  (list (cons 'very-naive v*-very-naive)
                      (cons 'naive v*-naive)
                      ;(cons 'dyn-prog (value-iteration 0.001))
                      )
                (list env1 env2 env3))

;; However the very naive recursion does not scale well at all!
;; Do not try this on the "larger" environment (in terms of actions and states)
(test-policies  (list (cons 'naive v*-naive)
                      ;(cons 'dyn-prog (value-iteration 0.001))
                      )
                (list env1 env2 env3 env4 env5 env6))