#lang racket

(provide compile***
         
         extend-syntactic-environment
         extend-syntactic-environment*
         add-identifier
         add-identifier-list
         filter-syntactic-env
         
         syntactic-closure?
         make-syntactic-closure
         make-syntactic-closure-list
         compile-syntactic-closure
         
         null-syntactic-environment
         core-syntactic-environment
         scheme-syntactic-environment)

;;  Alan Bawden and Jonathan Rees - Syntactic closures.

;; A /name/ is any token used to name something. e.g. quote car
;; A /keyword/ (or a /syntactic keyword/) is a name used to introduce some special syntactic construct. e.g. lambda set!
;; An /identifier/ is a named used to denote a variable. e.g. cdaadr foo
;; A /variable/ is a particular binding of an identifier. e.g. in (lambda (x) (f x (lambda (x) (g x))) there are two 
;;   variables named by x
;; A /syntactic environment/ maps identifiers to variables, gives interpretation of keywords: Contains all the info
;;   necessary for interpreting an expression
;; A /value environment/ maps variables to values (more precisely locations that hold the values). All the information
;;   necessary to execute an expression


;; compile*** macroexpands an expression in the given environment

(define (compile*** syntactic-env exp) (syntactic-env syntactic-env exp))
(define (compile***-list syntactic-env exps) (map (lambda (exp) (syntactic-env syntactic-env exp)) exps))


;; Syntactic Environments

(define (null-syntactic-environment syntactic-env exp)
  (if (syntactic-closure? exp)
      (compile-syntactic-closure syntactic-env exp)
      (error (list "Unclosed expression:" exp))))

(define (extend-syntactic-environment outer-syntactic-env keyword expander)
  ;; creates a new syntactic environment in which expressions (keyword ...) are expanded by the given expander
  ;; any other expression is interpreted using the previous environment
  (lambda (syntactic-env exp)
    (if (and (pair? exp) (eq? (car exp) keyword))
        (compile*** null-syntactic-environment (expander syntactic-env exp))
        (outer-syntactic-env syntactic-env exp))))

(define (extend-syntactic-environment* outer-syntactic-env keyword-expander-list)
  (lambda (syntactic-env exp)
    (cond ((and (pair? exp) (assq (car exp) keyword-expander-list))
           => (lambda (key-exp)
                (let ((expander (cdr key-exp)))
                  (compile*** null-syntactic-environment (expander syntactic-env exp)))))
          (else (outer-syntactic-env syntactic-env exp)))))

(define (add-identifier outer-syntactic-env identifier)
  (let ((variable (gensym identifier)))
    (lambda (syntactic-env exp)
      (if (eq? exp identifier)
          variable
          (if (and (pair? exp) (eq? (car exp) identifier)) ;; NOTE: Added this, it was missing from the paper.. weird?
              (cons variable (map (lambda (e) (outer-syntactic-env syntactic-env e)) (cdr exp))) ;; NOTE
              (outer-syntactic-env syntactic-env exp))))))

(define (add-identifier-list syntactic-env identifiers)
  (if (null? identifiers)
      syntactic-env
      (add-identifier (add-identifier-list syntactic-env (cdr identifiers))
                      (car identifiers))))

(define (filter-syntactic-env names names-syntactic-env else-syntactic-env)
  ;; creates a new syntactic environment in which a given list of names
  ;; take their meaning from one syntactic environment, while all others
  ;; take their meaning from the other
  (lambda (syntactic-env exp)
    ((if (memq (if (pair? exp) (car exp) exp) names)
         names-syntactic-env
         else-syntactic-env)
     syntactic-env exp)))


;; Syntactic Closures

(define (syntactic-closure? x)
  (and (vector? x) (= 2 (vector-length x)) (eq? 'syntactic-closure (vector-ref x 0))))

(define (make-syntactic-closure syntactic-env free-names exp)
  ;; returns a closure of the expression in the environment leaving the names free
  (vector 'syntactic-closure
          (lambda (free-names-syntactic-env)
            (compile*** (filter-syntactic-env free-names free-names-syntactic-env syntactic-env) exp))))

(define (make-syntactic-closure-list syntactic-env free-names exps)
  (map (lambda (exp) (make-syntactic-closure syntactic-env free-names exp)) exps))

(define (compile-syntactic-closure syntactic-env syntactic-closure)
  ((vector-ref syntactic-closure 1) syntactic-env))


;; Core syntactic environment.
;; 
;; Mostly an example, teaching how to use the system. Note that you only need
;; one 'core' syntactic environment that uses compile***, then extend it with
;; like how scheme-syntactic-environment is.

(define (core-syntactic-environment syntactic-env exp)
  ((cond ((syntactic-closure? exp) compile-syntactic-closure)
         ((symbol? exp) compile-free-variable)
         ((not (pair? exp)) compile-constant)
         (else (case (car exp)
                 ((quote) compile-constant)
                 ((if begin set!) compile-simple)
                 ((lambda) compile-lambda)
                 (else compile-combination))))
   syntactic-env exp))
(define (compile-constant syntactic-env exp) exp)
(define (compile-free-variable syntactic-env exp) exp)
(define (compile-simple syntactic-env exp) `(,(car exp) . ,(compile***-list syntactic-env (cdr exp))))
(define (compile-combination syntactic-env exp) (compile***-list syntactic-env exp))
(define (compile-lambda syntactic-env exp)
  (let ((syntactic-env (add-identifier-list syntactic-env (cadr exp))))
    `(lambda ,(compile***-list syntactic-env (cadr exp)) . ,(compile***-list syntactic-env (cddr exp)))))

;; Examples

(define (push-expander syntactic-env exp)
  (let ((obj-exp (make-syntactic-closure syntactic-env '() (cadr exp)))
        (list-var (make-syntactic-closure syntactic-env '() (caddr exp))))
    (make-syntactic-closure scheme-syntactic-environment '()
      `(set! ,list-var (cons ,obj-exp ,list-var)))))

(define (or-expander syntactic-env exp)
  (let ((exp-1 (make-syntactic-closure syntactic-env '() (cadr exp)))
        (exp-2 (make-syntactic-closure syntactic-env '() (caddr exp))))
    (make-syntactic-closure scheme-syntactic-environment '()
      `((lambda (temp)
          (if temp temp ,exp-2)) ,exp-1))))

(define (catch-expander syntactic-env exp)
  (let ((body-exp (make-syntactic-closure syntactic-env '(throw) (cadr exp))))
    (make-syntactic-closure scheme-syntactic-environment '()
      `(call-with-current-continuation
        (lambda (throw) ,body-exp)))))

(define scheme-syntactic-environment (extend-syntactic-environment* core-syntactic-environment
  `(;;(push! . ,push-expander)
    (or . ,or-expander)
    (catch . ,catch-expander))))


;; ;;(trace compile***)

;; ;(compile*** scheme-syntactic-environment '(push! foo ar))
;; (compile*** (extend-syntactic-environment scheme-syntactic-environment 'push! push-expander) '(cons (push! foo bar) (lambda (push!) (push! foo bar))))
;; (compile*** (extend-syntactic-environment scheme-syntactic-environment 'push! push-expander) '(lambda (throw) (throw (catch (begin foo (throw))))))
