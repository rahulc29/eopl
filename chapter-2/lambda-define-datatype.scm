#lang eopl
(define (identifier? symbol)
  (and (symbol? symbol)
       (not (eqv? 'lambda symbol))))
(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
(define (occurs-free? search-var exp)
  (cases lc-exp exp
    (var-exp (var) (eqv? var search-var))
    (lambda-exp (bound-var body)
                (and (not (eqv? search-var bound-var))
                     (occurs-free? search-var body)))
    (app-exp (rator rand)
             (or (occurs-free? search-var rator)
                 (occurs-free? search-var rand)))))