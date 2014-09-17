#lang racket

(provide apple apple? builtins interpret)

(require "pattern-matcher.rkt")

(define-language apple apple?
  (var symbol?)
  (nil null?)
  (quote `(quote ,apple?))
  (let `(let ((,symbol? ,apple?) ...) ,apple?))
  (if `(if ,apple? ,apple? ,apple?))
  (and `(and ,apple? ,apple?))
  (or `(or ,apple? ,apple?))
  (app `(,apple? ,apple? ...)))

(define builtins `((error . ,error)
                   
                   (eq? . ,eq?)
                   (symbol? . ,symbol?)
                   
                   (cons . ,cons)
                   (null? . ,null?)
                   (pair? . ,pair?)
                   
                   (car . ,car)
                   (cdr . ,cdr)
                   
                   (cadr . ,cadr)
                   (cddr . ,cddr)
                   (caar . ,caar)
                   (cdar . ,cdar)
                   
                   (caadr . ,caadr)
                   (caddr . ,caddr)
                   (cdaar . ,cdaar)
                   (cddar . ,cddar)))

(define (interpret env e)
  (match-language apple e
    (var => (lambda (v)
              (let ((b (assoc v env)))
                (if b
                    (cdr b)
                    (error (list "oh no unbound variable: " v))))))
    (nil => (lambda (n) n))
    (quote => (lambda (e) e))
    (let => (lambda (bindings body)
              (interpret (append (map (lambda (binding)
                                        (cons (car binding) (interpret env (cadr binding))))
                                      bindings)
                                 env)
                         body)))
    (if => (lambda (cond then else)
             (if (interpret env cond)
                 (interpret env then)
                 (interpret env else))))
    (and => (lambda (p q)
              (and (interpret env p) (interpret env q))))
    (or => (lambda (p q)
             (or (interpret env p) (interpret env q))))
    (app => (lambda (t ts)
              (let ((func (interpret env t))
                    (args (map (lambda (arg) (interpret env (car arg))) ts)))
                (apply func args))))))
