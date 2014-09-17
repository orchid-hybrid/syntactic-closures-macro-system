#lang racket

(require rackunit)

(require "interpreter.rkt")

(check-equal? (apple? 'foo) #t)
(check-equal? (apple? '(foo bar)) #t)
(check-equal? (apple? '(quote foo)) #t)
(check-equal? (apple? '(quote (foo bar))) #t)
(check-equal? (apple? '(let ((x 'apple) (y 'orange)) (cons x (cons y '())))) #t)
(check-equal? (interpret builtins '(cons 'x 'y)) '(x . y))
(check-equal? (interpret builtins '(car (cons 'x 'y))) 'x)
(check-equal? (interpret builtins '(cdr (cons 'x 'y))) 'y)
(check-equal? (interpret builtins '(let ((x 'apple) (y 'orange)) (cons y x))) '(orange . apple))

