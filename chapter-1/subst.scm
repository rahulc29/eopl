#lang eopl
; subst :: Symbol -> Symbol -> List
(define (subst new old slist)
  ; subst-symbol :: Symbol -> Symbol -> Symbol
  (define (subst-symbol new old exp)
    (if (eqv? exp old)
        new
        exp))
  ; subst-sexp :: Symbol -> Symbol -> Sexp
  (define (subst-sexp new old exp)
    (if (symbol? exp)
        (subst-symbol new old exp)
        (subst-slist new old exp)))
  ; subst-slist :: Symbol -> Symbol -> Slist -> Slist
  (define (subst-slist new old slist)
    (map (lambda (exp) (subst-sexp new old exp))
         slist))
  (subst-slist new old slist))
  
