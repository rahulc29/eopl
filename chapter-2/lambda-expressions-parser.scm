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
(define (parse-expression data)
  (cond
    ((symbol? data) (var-exp data))
    ((pair? data)
     (if (eqv? (car data) 'lambda)
         (lambda-exp
          (car (cadr data))
          (parse-expression (caddr data)))
         (app-exp
          (parse-expression (car data))
          (parse-expression (cadr data)))))
    (else (eopl:error 'parse-expression "Invalid concrete syntax"))))
(define (unparse-lc-exp exp)
  (cases lc-exp exp
    (var-exp (var) var)
    (lambda-exp (bound-var body)
                (list 'lambda (list bound-var)
                      (unparse-lc-exp body)))
    (app-exp (rator rand)
             (list (unparse-lc-exp rator)
                   (unparse-lc-exp rand)))))
     