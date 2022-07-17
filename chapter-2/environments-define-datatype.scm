#lang eopl
(define-datatype environment environment?
  (empty-env)
  (extended-env
   (binding binding?)
   (subenv environment?)))
(define (binding? object)
  (and (pair? object)
       (symbol? (car object))))
