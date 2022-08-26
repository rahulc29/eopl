#lang eopl
(define-datatype desugared-tree term?
  (int-exp
   (val integer?))
  (bool-exp
   (val (lambda (val) (or (eq? val #t)
                          (eq? val #f)))))
  (var-exp
   (var symbol?))
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
   (arguments (lambda (x) (or (pair? x) (null? x)))))
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
(provide nary-exp)
(provide proc-exp)
(provide call-exp)
(provide var-exp)
(provide desugared-tree)