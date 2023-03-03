#lang r7rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                             Logger                              *-*-
;-*-*                                                                 *-*-
;-*-*                           Youri Coppens                         *-*-
;-*-*                 2022 Artificial Intelligence Lab                *-*-
;-*-*                    Vrije Universiteit Brussel                   *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

;; A simple logging facility

;; Inspired by work from the Chicken Scheme module of Tom Kwong and contributors.
;; Licensed under MIT License.

;; Sample usage:
;;
;; ; set your logging level to one of the following
;; ; 'debug 'info 'warning 'error
;; (log-set-verbosity! 'debug)
;; ; log a message
;; (log-debug "Here's a debugging message.")
;; (log-info "Just an FYI: That thing happened.")
;; (log-warning "Something bad happened, but i handled it.")
;; (log-error "Something bad happened and I don't feel good.")
;;
(define-library (logger)
  (export log-get-verbosity log-set-verbosity! log-debug log-info log-warning log-error)
  (import (scheme base)
          (scheme write)
          (only (racket base) seconds->date current-seconds)
          (only (racket date) date-display-format date->string))
  (begin

    ;; Default values
    (define default-verbosity-level 2) ; 0=error 1=warning 2=info 3=debug
    (define default-output-port (current-output-port))

    ;; Private procedure that logs data into the output port
    (define (%default-logger . args)
      (for-each (lambda (x) 
                  (display x output-port)
                  (display " " output-port))
                (append 
                 (list (%current-datetime-string))
                 args))
      (display #\newline output-port)
      (flush-output-port output-port)
      #t)

    ;; Global variables
    (define verbosity-level default-verbosity-level)
    (define output-port default-output-port)
    (define logger %default-logger)

    ;; Private procedure to capture time into a string
    (define (%current-datetime-string)
      (date-display-format 'iso-8601)
      (date->string (seconds->date (current-seconds))))

    (define (log-debug . args) 
      (if (> verbosity-level 2)
          (apply logger (cons "DEBUG  " args))
          #f))

    (define (log-info . args) 
      (if (> verbosity-level 1)
          (apply logger (cons "INFO   " args))
          #f))

    (define (log-warning . args)
      (if (> verbosity-level 0)
          (apply logger (cons "WARNING" args))
          #f))

    (define (log-error . args) 
      (apply logger (cons "ERROR  " args)))

    (define (log-get-verbosity)
      verbosity-level)

    (define (log-set-verbosity! v)
      (let* ((sym (list (cons 'error 0) (cons 'warning 1) (cons 'info 2) (cons 'debug 3)))
             (idx (assq v sym)))
        (if idx
            (set! verbosity-level (cdr idx))
            (log-error "invalid verbosity label: " v "; it must be one of '(" sym ")"))))
    ))
