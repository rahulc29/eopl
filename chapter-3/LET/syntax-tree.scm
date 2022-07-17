#lang eopl
(define-datatype program program?
  (int-exp
   (val integer?))
  (bool-exp
   (val (lambda (val) (or (eq? val #t)
                          (eq? val #f)))))
  (var-exp
   (var symbol?))
  (diff-exp
   (left program?)
   (right program?))
  (zero?-exp
   (predicate program?))
  (if-exp
   (antecedent program?)
   (consequent program?)
   (else-clause program?))
  (let-exp
   (variable symbol?)
   (variable-eval-exp program?)
   (in-clause program?)))
(provide int-exp)
(provide bool-exp)
(provide let-exp)
(provide if-exp)
(provide zero?-exp)
(provide diff-exp)
(provide var-exp)
(provide program)