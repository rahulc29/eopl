#lang eopl
(define (var-exp var)
  (lambda (signal)
    (cond
      ((eqv? signal 'var-exp?) #t)
      ((eqv? signal 'lambda-exp?) #f)
      ((eqv? signal 'app-exp?) #f)
      ((eqv? signal 'var-exp->var) var)
      (else (eopl:error 'var-exp "Undefined method")))))
(define (lambda-exp var body)
  (lambda (signal)
    (cond
      ((eqv? signal 'var-exp?) #f)
      ((eqv? signal 'lambda-exp?) #t)
      ((eqv? signal 'app-exp?) #f)
      ((eqv? signal 'lambda-exp->bound-var) var)
      ((eqv? signal 'lambda-exp->body) body)
      (else (eopl:error 'lambda-exp "Undefined method")))))
(define (app-exp rator rand)
  (lambda (signal)
    (cond
      ((eqv? signal 'var-exp?) #f)
      ((eqv? signal 'lambda-exp?) #f)
      ((eqv? signal 'app-exp?) #t)
      ((eqv? signal 'app-exp->rator) rator)
      ((eqv? signal 'app-exp->rand) rand)
      (else (eopl:error 'app-exp "Undefined method")))))
(define (var-exp? exp)
  (exp 'var-exp?))
(define (lambda-exp? exp)
  (exp 'lambda-exp?))
(define (app-exp? exp)
  (exp 'app-exp?))
(define (var-exp->var exp)
  (exp 'var-exp->var))
(define (lambda-exp->bound-var exp)
  (exp 'lambda-exp->bound-var))
(define (lambda-exp->body exp)
  (exp 'lambda-exp->body))
(define (app-exp->rator exp)
  (exp 'app-exp->rator))
(define (app-exp->rand exp)
  (exp 'app-exp->rand))
(define (occurs-free? search-var exp)
  (cond
    ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
    ((lambda-exp? exp)
     (and (not (eqv? search-var (lambda-exp->bound-var exp)))
          (occurs-free? search-var (lambda-exp->body exp))))
    (else
     (or (occurs-free? search-var (app-exp->rator exp))
         (occurs-free? search-var (app-exp->rand exp))))))