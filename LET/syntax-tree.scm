#lang eopl
(define-datatype program program?
  (int-exp
   (val integer?))
  (bool-exp
   (val (lambda (val) (or (eq? val #t)
                          (eq? val #f)))))
  (var-exp
   (var symbol?))
  (nullary-op-exp
   (operator symbol?))
  (unary-op-exp
   (operator symbol?)
   (argument program?))
  (binop-exp
   (operator symbol?)
   (left program?)
   (right program?))
  (if-exp
   (antecedent program?)
   (consequent program?)
   (else-clause program?))
  (nary-exp
   (operator symbol?)
   (operands pair?))
  (let*-exp
   (var-list pair?)
   (expr program?))
  (let-exp
   (var-list pair?)
   (expr program?)))
(provide int-exp)
(provide bool-exp)
(provide let-exp)
(provide let*-exp)
(provide if-exp)
(provide unary-op-exp)
(provide binop-exp)
(provide nary-exp)
(provide nullary-op-exp)
(provide var-exp)
(provide program)