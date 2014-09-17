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
            '(begin
               (push! x y)
               
               (define-syntax quasiquote
                 (lambda (syntactic-env exp)
                   (let ((body (cadr exp)))
                     (if (or (null? body) (symbol? body))
                         (make-syntactic-closure scheme-macro-environment '()
                                                 (cons 'quote (cons body '())))
                         (if (pair? body)
                             (if (eq? (car body) 'unquote)
                                 (make-syntactic-closure scheme-macro-environment '()
                                                         (cadr body))
                                 (make-syntactic-closure scheme-macro-environment '()
                                                         (cons 'cons (cons (cons 'quasiquote (cons (car body) '()))
                                                                           (cons (cons 'quasiquote (cons (cdr body) '()))
                                                                                 '())))))
                             (error body))))))

               (define-syntax push!
                 (lambda (syntactic-env exp)
                   (let ((obj-exp (make-syntactic-closure syntactic-env '() (cadr exp)))
                         (list-var (make-syntactic-closure syntactic-env '() (caddr exp))))
                     (make-syntactic-closure scheme-macro-environment '()
                                             ;;`(set! ,list-var (cons ,obj-exp ,list-var))
                                             (cons 'set! (cons list-var (cons (cons 'cons (cons obj-exp (cons list-var '()))) '())))
                                             ))))
               
               (push! x y)
               (define foo `(foo ,bar (push! baz bar) ,(push! baz bar) ,quux))
               (lambda (push!) (push! x y))))
