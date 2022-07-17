#lang eopl
(define-datatype program program?
  (const-int-exp
   (val integer?))
  (const-bool-exp
   (val (lambda (val) (or (eq? val #t)
                          (eq? val #f)))))
  (diff-exp
   (left program?)
   (right program?))
  (zero-test-exp
   (predicate program?))
  (conditional-exp
   (antecedent program?)
   (consequent program?)
   (else-clause program?))
  (let-exp
   (variable symbol?)
   (variable-eval-exp program?)
   (in-clause program?)))
(define (num-val int)
  (const-int-exp int))
(define (bool-val bool)
  (const-bool-exp bool))
(define (expval->num exp)
  (cases program exp
    (const-int-exp (val) (list 'some val))
    (else '(none))))
(define (expval->bool exp)
  (cases program exp
    (const-bool-exp (val) (list 'some val))
    (else '(none))))
