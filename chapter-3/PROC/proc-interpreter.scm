#lang eopl
(require "procedural-environment.scm")
(require "proc-parser.scm")
(require "syntax-tree.scm")
; TODO : Refactor code so that pre-defined operators (=,+,*,<, etc.) become pre-defined procedures
; in an initial environment
(define (init-env)
  (empty-env))
(define (value-of exp env)
  (define (extract-binary op)
    (cond
      ((eqv? op '+) +)
      ((eqv? op '*) *)
      ((eqv? op '-) -)
      ((eqv? op 'equals?)
       (lambda (x y) (= x y)))
      ((eqv? op 'greater?) >)
      ((eqv? op 'less?) <)
      ((eqv? op 'cons)
       cons)))
  (define (extract-unary op)
    (cond
      ((eqv? op 'negate)
       (lambda (x) (- x)))
      ((eqv? op 'zero?)
       (lambda (x) (zero? x)))
      ((eqv? op 'car) car)
      ((eqv? op 'cdr) cdr)))
  (define (extract-nullary op)
    (cond
      ((eqv? op 'emptylist) '())))
  (define (extract-nary op)
    (define (list-evaluator args env)
      (map (lambda (arg) (value-of arg env)) args))
    (define (cond-evaluator conds env)
      (define (extract-predicate cond-cell)
        (list-ref cond-cell 0))
      (define (extract-expression cond-cell)
        (list-ref cond-cell 1))
      (if (value-of (extract-predicate (car conds)) env)
          (value-of (extract-expression (car conds)) env)
          (cond-evaluator (cdr conds) env)))
    (cond
      ((eqv? op 'list) list-evaluator)
      ((eqv? op 'cond) cond-evaluator)))
  (define (extend-env-varlist-let* varlist env)
    (define (extend-env-var var-cell env)
      (define (extract-var var-cell)
        (list-ref var-cell 0))
      (define (extract-expr var-cell)
        (list-ref var-cell 1))
      (extend-env (extract-var var-cell)
                  (value-of (extract-expr var-cell)
                            env)
                  env))
    (if (null? varlist)
        env
        (extend-env-varlist-let* (cdr varlist)
                                 (extend-env-var (car varlist) env))))
  (define (extend-env-varlist-let varlist env)
    (define (extract-var var-cell)
      (list-ref var-cell 0))
    (define (extract-expr var-cell)
      (list-ref var-cell 1))
    (define (value-of-var-decl var-cell)
      (list (extract-var var-cell)
            (value-of (extract-expr var-cell) env)))
    (define (extend-env-vars vars env)
      (if (null? vars)
          env
          (extend-env-vars (cdr vars)
                           (extend-env (extract-var (car vars))
                                       (extract-expr (car vars))
                                       env))))
    (extend-env-vars (map value-of-var-decl varlist)
                     env))
  (cases program exp
    (int-exp (val) val)
    (bool-exp (val) val)
    (var-exp (var) (apply-env env var))
    (binop-exp (op left right) ((extract-binary op)
                                (value-of left env)
                                (value-of right env)))
    (unary-op-exp (op arg) ((extract-unary op)
                            (value-of arg env)))
    (nary-exp (op args) ((extract-nary op) args env))
    (if-exp (cond then else)
            (if (value-of cond env)
                (value-of then env)
                (value-of else env)))
    ; TODO : Implement evaluation of proc-exp and call-exp
    (proc-exp (param body) '())
    (call-exp (rator rand) '())
    (nullary-op-exp (op)
                    (extract-nullary op))
    (let*-exp (varlist body)
              (value-of body (extend-env-varlist-let* varlist env)))
    (let-exp (varlist body)
             (value-of body (extend-env-varlist-let varlist env)))))
(define (run program)
  (value-of (parse-tree program) (init-env)))