#lang eopl
; lambda-variable :: List -> Symbol
(define (lambda-variable exp)
  (caadr exp))
; lambda-body :: List -> List
(define (lambda-body exp)
  (cddr exp))
; lambda? :: List -> Bool
(define (lambda? exp)
  (eqv? 'lambda (car exp)))
; variable? :: Any -> Bool
(define (variable? exp)
  (symbol? exp))
(define (application-procedure exp)
  (car exp))
(define (application-argument exp)
  (cdr exp))
; occurs? :: Symbol -> List -> Bool
(define (occurs? var exp)
  (define (occurs-in-lambda? var exp)
    (or (occurs? var (lambda-variable exp))
        (occurs? var (lambda-body exp))))
  (cond
    ((variable? exp) (eqv? var exp))
    ((lambda? exp) (occurs-in-lambda? var exp))
    (else (or (occurs? var (car exp))
              (occurs? var (cdr exp))))))
; occurs-free? :: Symbol -> List -> Bool
(define (occurs-free? var exp)
  (define (occurs-free-in-lambda? var exp)
    (and (not (occurs-free? var (lambda-variable exp)))
         (occurs-free? var (lambda-body exp))))
  (cond
    ((variable? exp) (eqv? var exp))
    ((lambda? exp) (occurs-free-in-lambda? var exp))
    (else (or (occurs-free? var (application-procedure exp))
              (occurs-free? var (application-argument exp))))))
; type Slist = () | (Sexp . Slist)
; and  Sexp  = Symbol | Slist
; slist-sexp :: Slist -> Sexp
(define (slist-sexp exp)
  (car exp))
; slist-sublist :: Slist -> Slist
(define (slist-sublist exp)
  (cdr exp))
; subst :: Symbol -> Symbol -> List -> List
(define (subst new old exp)
  (define (subst-symbol new old exp)
    (if (eqv? exp old)
        new
        exp))
  (define (subst-slist new old exp subst-sexp)
    (if (null? exp)
        '()
        (cons (subst-sexp new old (slist-sexp exp) subst-slist)
              (subst-slist new old (slist-sublist exp) subst-sexp))))
  (define (subst-sexp new old exp subst-slist)
    (if (symbol? exp)
        (subst-symbol new old exp)
        (subst-slist new old exp subst-sexp)))
  (subst-sexp new old exp subst-slist))