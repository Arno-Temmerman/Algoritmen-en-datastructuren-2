#lang r7rs
(define-library (gridworld-mdp)
  (export make-env gridworld? num-cols num-rows actions action-set discount action-name
          make-2D-cell get-x get-y same-state? terminal? num-states start goal
          make-action get-x-step get-y-step same-action?
          manh-acts manh-names manh-steps
          king-acts king-names king-steps
          transition reward)
  (import (scheme base))
  (begin

    ;;; 2D Gridworld
    ;     -----------------------------> X
    ;    |  -------------------------
    ;    |  | start |  ...  |       |
    ;    |  ----------     ----------
    ;    |  |       |  ...  |       | 
    ;    |  ----------     ----------
    ;    |  |       |  ...  |  goal |
    ; Y  |  -------------------------
    ;    v

    ; Descriptive information on the instance of the environment kept in the record
    (define-record-type gridworld-environment
      (make-grid c r a as d)
      gridworld?
      (c num-cols)
      (r num-rows)
      (a actions)
      (as action-set)
      (d discount))

    (define (make-env cols rows act-set disc)
      (make-grid cols rows (map car act-set) act-set disc))

    (define (action-name env move)
      (cdr (assoc move (action-set env))))
 
    ;; Defining the dynamics of our gridworld problem as a Markov Decision Process (MDP)

    ; States: 2D-coordinates in a grid aka cells of a matrix
    (define make-2D-cell cons)
    (define get-x car) ; column
    (define get-y cdr) ; row
    (define same-state? equal?)

    (define (num-states env) (* (num-cols env) (num-rows env)))

    (define (start env) (make-2D-cell 0 0))
    (define (goal env) (make-2D-cell (- (num-cols env) 1)
                                     (- (num-rows env) 1)))

    (define (terminal? env state)
      (same-state? state (goal env)))

    ; Actions: steps in the gridworld, represented by 2D unit vectors (mathematical vector!)
    (define make-action cons)
    (define get-x-step car)
    (define get-y-step cdr)
    (define same-action? equal?)

    ; Action set: a collection of allowed actions in the environment
    ; Represented as an association list of actions (key) and their name (value) 

    ; Pre-made action sets

    ; Manhattan Steps: up, left, down, right
    (define manh-acts (list (make-action 0 -1) ; see sketch above: y-axis goes downwards!
                            (make-action -1 0)
                            (make-action 0 1)
                            (make-action 1 0))) ; see sketch above: y-axis goes downwards!                

    (define manh-names '(↑ ← ↓ →))
   
    (define manh-steps (map cons manh-acts manh-names))

    ; King's moves: all 8 possible directions in the grid
    (define king-acts (list (make-action 0 1)
                            (make-action 0 -1)
                            (make-action 1 0)
                            (make-action -1 0)
                            (make-action 1 1)
                            (make-action 1 -1)
                            (make-action -1 1)
                            (make-action -1 -1)))

    (define king-names '(↓ ↑ → ← ↘ ↗ ↙ ↖))

    (define king-steps (map cons king-acts king-names))

    ; Transitions: given a state and an action, move to a neighboring cell in the grid unless we reach a border
    (define (transition env state action)
      ; Auxiliary procedure to restrain a value x to an interval [a, b]
      (define (clip x a b)
        (if (< x a)
            a
            (if (< x b)
                x
                b)))
      (if (terminal? env state)
          state
          (let ((new-x (+ (get-x state) (get-x-step action)))
                (new-y (+ (get-y state) (get-y-step action))))
            (make-2D-cell (clip new-x 0 (- (num-cols env) 1))
                          (clip new-y 0 (- (num-rows env) 1))))))

    ; Reward: what are good actions? What are bad ones?
    ; Punishment strategy: get a penalty for each action you take not leading to the goal.
    (define (reward env state action next-state)
      (if (terminal? env next-state)
          0
          -1))
    ))