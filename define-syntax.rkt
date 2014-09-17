#lang racket

(require "pattern-matcher.rkt"
         "syntactic-closures.rkt"
         "interpreter.rkt")

(define *macros* '())

(define (extensible-syntactic-environment outer-syntactic-env)
  ;; creates a new syntactic environment in which expressions (keyword ...) are expanded by the given expander
  ;; any other expression is interpreted using the previous environment
  (lambda (syntactic-env exp)
    (cond ((and (pair? exp) (assq (car exp) *macros*))
           => (lambda (expander-pair)
                (compile*** null-syntactic-environment ((cdr expander-pair) syntactic-env exp))))
          (else (outer-syntactic-env syntactic-env exp)))))


(define (apply-list-to f) (lambda (l) (apply f l)))
(define (define-syntax-expander syntactic-env exp)
  ;;(display exp)(newline)
  ;;(display (pattern? `(define-syntax ,symbol? (lambda (,symbol? ,symbol?) _)) exp)) (newline)
  (cond ((pattern? `(define-syntax ,symbol? (lambda (,symbol? ,symbol?) _)) exp)
         => (apply-list-to
             (lambda (macro-name syntactic-env-name exp-name body)
               (let ((macro (cons macro-name (lambda (syntactic-env-value exp-value)
                                               (interpret (append `((,syntactic-env-name . ,syntactic-env-value)
                                                                    (,exp-name . ,exp-value))
                                                                  macro-expansion-builtins)
                                                          body)))))
                 (set! *macros* (cons macro *macros*)))
               (make-syntactic-closure scheme-syntactic-environment '()
                                       `(begin)))))
        (error (list "Malformed define-syntax" exp))))

(define scheme-macro-environment
  (extensible-syntactic-environment
   (extend-syntactic-environment scheme-syntactic-environment 'define-syntax define-syntax-expander)))

(define macro-expansion-builtins
  (append `((make-syntactic-closure . ,make-syntactic-closure)
            (scheme-macro-environment . ,scheme-macro-environment))
          builtins))

(compile*** scheme-macro-environment
            `(begin
               (push! x y)
               
               (define-syntax push!
                 (lambda (syntactic-env exp)
                   (let ((obj-exp (make-syntactic-closure syntactic-env '() (cadr exp)))
                         (list-var (make-syntactic-closure syntactic-env '() (caddr exp))))
                     (make-syntactic-closure scheme-macro-environment '()
                                             (cons 'set! (cons list-var (cons (cons 'cons (cons obj-exp (cons list-var '()))) '())))))))
               
               (push! x y)))
