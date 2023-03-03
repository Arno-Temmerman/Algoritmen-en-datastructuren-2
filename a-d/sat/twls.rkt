#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                    Two-watched Literal Scheme                   *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                           Bart Bogaerts                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(define-library (twls)
  (export new reason reason! level level! time time! times
          remove-clause-from-var-watch! add-clause-to-var-watch!
          change-watch! other-watch find-unwatched-nonfalse-lit add-clause!
          get-lit-watchers clause)
  (import (scheme base)    
          (a-d sat cnf)
          (a-d sat logger)
          (a-d sat interpretation)
          (a-d scheme-tools))
  (begin

    ; Wrapper around normal clause to incorporate watches
    (define-record-type watched-clause
      (make-watched-clause cl fw sw)
      watched-clause?
      (cl clause) ; the original clause
      (fw first-watch first-watch!)
      (sw second-watch second-watch!))

    (define (from-clause cls)
      (if (> (clause-size cls) 1)
          (make-watched-clause cls
                               (car (literals cls))
                               (cadr (literals cls)))
          #f))
 
    (define (find-unwatched-nonfalse-lit wcls interpret)
      (call-with-current-continuation
       (lambda (exit)
         (for-each (lambda (lit)
                     (if (and (not (same-literal? lit (first-watch wcls)))
                              (not (same-literal? lit (second-watch wcls)))
                              (not (false? interpret lit)))
                         (exit lit)))
                   (literals (clause wcls)))
         #f)))
 
    ; change the c-watch (first or second) to another literal
    (define (change-watch! wcls from-lit to-lit)
      (if (same-literal? (first-watch wcls)
                         from-lit)
          (first-watch! wcls to-lit)
          (second-watch! wcls to-lit)))

    (define (other-watch wcls l)
      (if (same-literal? (first-watch wcls) l)
          (second-watch wcls)
          (first-watch wcls)))
 
    (define-record-type twls
      (make-twls vw wc d r t)
      twls?
      (vw var-watchers)
      (wc watched-clauses watched-clauses!)
      (d decision-levels)
      (r reasons)
      (t times))

    (define (new f)
      (let* ((watched-cls (fold-right (lambda (elem acc)
                                        (let ((res (from-clause elem)))
                                          (if res
                                              (cons res acc)
                                              acc)))
                                      '()
                                      (clauses f)))
             (var-watches  (vector-map (lambda (x) (make-var-watch '() '()))
                                       (make-vector (number-of-vars f)))) ; !!!! vector-map in order to have separate objects 
             (reasons  (make-vector (number-of-vars f) '()))
             (dec-levels  (make-vector (number-of-vars f) -1))
             (times  (make-vector (number-of-vars f) #f))
             (twls (make-twls var-watches
                              watched-cls
                              reasons
                              dec-levels
                              times)))
        (for-each (lambda (wcls)
                    (for-each (lambda (w)
                                (add-clause-to-var-watch! twls w wcls))
                              (list (first-watch wcls)
                                    (second-watch wcls))))
                  watched-cls)
        twls))
 
    ;; mini-abstraction
    (define pos-lit car)
    (define neg-lit cdr)

    (define (level twls lit)
      (vector-ref (decision-levels twls) (- (variable lit) 1)))
   
    (define (level! twls lit level)
      (vector-set! (decision-levels twls) (- (variable lit) 1) level))

    (define (reason twls lit)
      (vector-ref (reasons twls) (- (variable lit) 1)))
 
    (define (reason! twls lit reason)
      (vector-set! (reasons twls) (- (variable lit) 1) reason))

    (define (time twls lit)
      (vector-ref (times twls) (- (variable lit) 1)))
   
    (define (time! twls lit time)
      (vector-set! (times twls) (- (variable lit) 1) time))

    (define (add-clause! twls cls)
      (let ((wcls (from-clause cls)))
        (if wcls
            (begin
              (watched-clauses! twls (cons wcls (watched-clauses twls)))
              (add-clause-to-var-watch! twls (first-watch wcls) wcls)
              (add-clause-to-var-watch! twls (second-watch wcls) wcls))
            #f)))

    ;;; var-watch abstraction: a pair of lists
    (define make-var-watch cons)

    (define pos-lit! set-car!)
    (define neg-lit! set-cdr!)

    (define (get-var-watchers twls lit)
      (vector-ref (var-watchers twls) (- (variable lit) 1)))
 
    (define (get-lit-watchers twls lit)
      (if (negation? lit)
          (neg-lit (get-var-watchers twls lit))
          (pos-lit (get-var-watchers twls lit))))
 
    (define (add-clause-to-var-watch! twls lit watched-cls)
      (log-debug "Adding" (clause->string (clause watched-cls)) "to" (literal->string lit))
      (if (negation? lit)
          (neg-lit! (get-var-watchers twls lit) (cons watched-cls (get-lit-watchers twls lit)))
          (pos-lit! (get-var-watchers twls lit) (cons watched-cls (get-lit-watchers twls lit)))))

    (define (remove-clause-from-var-watch! twls lit watched-cls)
      (log-debug "Removing" (clause->string (clause watched-cls)) "from" (literal->string lit)"'s list...")
      (log-debug "Before:" (map (lambda (wcls) (clause->string (clause wcls))) (get-lit-watchers twls lit)))
      ((if (negation? lit)
           neg-lit!
           pos-lit!)
       (get-var-watchers twls lit)
       (remove watched-cls (get-lit-watchers twls lit)))
      (log-debug "After:" (map (lambda (wcls) (clause->string (clause wcls))) (get-lit-watchers twls lit))))
    ))