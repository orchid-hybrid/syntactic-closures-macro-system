#lang racket

(provide pattern? match
         language-pattern-names language-patterns define-language match-language)

(require racket/set)
(require (for-syntax racket/set))
(require srfi/1)

(define (pattern? p e)
  ;; (display `(pattern? ',p ',e))(newline)
  (cond ((null? p) (if (null? e) '() #f))
        ((equal? p '_) (list e))
        ((or (symbol? p)
             (char? p)) (if (equal? p e) '() #f))
        ((procedure? p) (if (p e) (list e) #f))
        ((pair? p) (if (pair? e)
                       (let ((lhs (pattern? (car p) (car e))))
                         (if lhs 
                             (if (equal? '(...) (cdr p))
                                 (pattern* (car p) (cdr e) (list lhs))
                                 (let ((rhs (pattern? (cdr p) (cdr e))))
                                   (if rhs (append lhs rhs)
                                       #f))) #f))
                       (if (equal? '(...) (cdr p))
                           (list '())
                           #f)))
        (else (error "bad pattern"))))

(define (pattern* p e acc)
  ;; (display `(pattern* ',p ',e ',acc))(newline)
  (if (null? e)
      (list (reverse acc))
      (if (pair? e)
          (let ((next (pattern? p (car e))))
            (if next
                (pattern* p (cdr e) (cons next acc))
                #f))
          #f)))

(define (apply-list-to f) (lambda (l) (apply f l)))

(define-syntax match
  (syntax-rules (=> else)
    ((match <exp> (<pattern> => <result>) ... (else <else>))
     (cond ((pattern? <pattern> <exp>) => (apply-list-to <result>)) ... (else <else>)))
    ((match <exp> (<pattern> => <result>) ...)
     (cond ((pattern? <pattern> <exp>) => (apply-list-to <result>)) ...))))

(define (language-pattern-names language) (map car language))
(define (language-patterns language) (map cdr language))
(define-syntax define-language
  (syntax-rules ()
    ((_ <language> <language?> (<name> <pattern>) ...)
     (begin
       (begin-for-syntax (define <language> `(<name> ...)))
       (define (<language>) `((<name> ,<pattern>) ...))
       (define (<language?> t)
         (if (any (lambda (pattern) (pattern? pattern t)) `(,<pattern> ...))
             #t #f))))))

(begin-for-syntax
  (define (match-language-verify lang lang-symbol pattern-names)
    (let* ((missing-clauses (set-subtract lang pattern-names))
           (extra-clauses (set-subtract pattern-names lang)))
      (if (not (null? missing-clauses))
          (error "Missing clauses in pattern match for language " lang-symbol ': missing-clauses)
          (when (not (null? extra-clauses))
            (error "Extra clauses in pattern match for language " lang-symbol ': extra-clauses))))))

(define-syntax match-language
  (lambda (stx)
    (syntax-case stx (->)
      
      ((_ (<language> -> <out-predicate>) <exp> . <rules>)
       (match-language-verify (eval #'<language>) '<language> (map car (syntax->datum #'<rules>)))
       #'(let ((result (match-language-aux <language> <exp> . <rules>)))
           (unless (<out-predicate> result)
             (error "Invalid data from pattern match, wanted" '<out-predicate> "got" result))
           result))
      
      ((_ <language> <exp> . <rules>)
       (match-language-verify (eval #'<language>) '<language> (map car (syntax->datum #'<rules>)))
       #'(match-language-aux <language> <exp> . <rules>))
      
      )))

(define-syntax match-language-aux
  (syntax-rules (=>)
    ((_ <language> <exp> (<name> => <k>) ...)
     (let ((e <exp>))
       (match e
         ((second (assoc '<name> (<language>))) => <k>) ...
         (else (error "Pattern match failed for " '<language> "with" e)))))))
