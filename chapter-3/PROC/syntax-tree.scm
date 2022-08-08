#lang eopl
(define-datatype program term?
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
   (argument term?))
  (binop-exp
   (operator symbol?)
   (left term?)
   (right term?))
  (if-exp
   (antecedent term?)
   (consequent term?)
   (else-clause term?))
  (nary-exp
   (operator symbol?)
   (operands pair?))
  (proc-exp
   (parameters pair?)
   (body term?))
  (call-exp
   (name term?)
   (arguments pair?))
  (let*-exp
   (var-list pair?)
   (expr term?))
  (let-exp
   (var-list pair?)
   (expr term?)))
(provide term?)
(provide int-exp)
(provide bool-exp)
(provide let-exp)
(provide let*-exp)
(provide if-exp)
(provide unary-op-exp)
(provide proc-exp)
(provide call-exp)
(provide binop-exp)
(provide nary-exp)
(provide nullary-op-exp)
(provide var-exp)
(provide program)