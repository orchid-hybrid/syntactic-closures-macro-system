#lang racket

(provide apple apple? builtins interpret)

(require "pattern-matcher.rkt")

(define-language apple apple?
  (var symbol?)
  (nil null?)
  (quote `(quote ,apple?))
  (let `(let ((,symbol? ,apple?) ...) ,apple?))
  (app `(,apple? ,apple? ...)))

(define builtins `((cons . ,cons)
                   (car . ,car)
                   (cdr . ,cdr)))

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
    (app => (lambda (t ts)
              (let ((func (interpret env t))
                    (args (map (lambda (arg) (interpret env (car arg))) ts)))
                (apply func args))))))

; (interpret '(cons 'x 'y)) '(x . y)
; (interpret '(car (cons 'x 'y))) 'x
; (interpret builtins '(let ((x 'apple) (y 'orange)) (cons x y))) '(apple . orange)
