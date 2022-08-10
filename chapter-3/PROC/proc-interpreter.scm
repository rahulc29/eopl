#lang eopl
(require "procedural-environment.scm")
(require "proc-parser.scm")
(require "syntax-tree.scm")
; TODO : Refactor code so that pre-defined operators (=,+,*,<, etc.) become pre-defined procedures
; in an initial environment
(define (extend-env-with-key-value-lists env keys values)
  (if (null? values)
      env
      (extend-env-with-key-value-lists (extend-env (car keys)
                                                   (car values)
                                                   env)
                                       (cdr keys)
                                       (cdr values))))
(define (extend-env-with-key-value-pairs env pairs)
  (if (null? pairs)
      env
      (extend-env-with-key-value-pairs (extend-env (car (car pairs))
                                                   (cdr (car pairs))
                                                   env)
                                       (cdr pairs))))
(define (init-env)
  (define (make-meta proc)
    (meta-procedure
     (lambda (env args)
       (proc args))))
  (define (make-foldr op id)
    (define (internal-foldr op id args)
      (if (null? args)
          id
          (op (car args)
              (internal-foldr op id (cdr args)))))
    (meta-procedure
     (lambda (env args)
       (internal-foldr op id args))))
  (define (make-foldl op id)
    (define (internal-foldl op acc args)
      (if (null? args)
          acc
          (internal-foldl op (op acc (car args)) (cdr args))))
    (meta-procedure
     (lambda (env args)
       (internal-foldl op id args))))
  (define (make-sort-checker op)
    (define (internal-sort-checker op curr args acc)
      (if (null? args)
          acc
          (internal-sort-checker op
                                 (car args)
                                 (cdr args)
                                 (and acc (op curr (car args))))))
    (meta-procedure
     (lambda (env args)
       (if (null? args)
           #t
           (internal-sort-checker op
                                  (car args)
                                  (cdr args)
                                  #t)))))
  (define (make-binary op)
    (meta-procedure
     (lambda (env args)
       (if (= 2 (length args))
           (op (list-ref args 0)
               (list-ref args 1))
           (eopl:error 'primitive-procedure "Binary operator should be given 2 operands")))))
  (define (make-unary op)
    (meta-procedure
     (lambda (env args)
       (if (= 1 (length args))
           (op (list-ref args 0))
           (eopl:error 'primitive-procedure "Unary operator should be given 1 operand")))))

  (define primitives
    (list (cons '+ (make-foldr + 0))
          (cons '* (make-foldr * 1))
          (cons '- (make-foldr - 0))
          (cons 'cons (make-binary cons))
          (cons 'car (make-unary car))
          (cons 'cdr (make-unary cdr))
          (cons 'negate (make-unary (lambda (x) (- x))))
          (cons 'zero? (make-unary zero?))
          (cons 'null? (make-unary null?))
          (cons 'list (make-meta (lambda (x) x)))
          (cons 'emptylist (make-meta (lambda (x) '())))
          (cons '< (make-sort-checker <))
          (cons '> (make-sort-checker >))
          (cons '>= (make-sort-checker >=))
          (cons '<= (make-sort-checker <=))))
  (extend-env-with-key-value-pairs (empty-env)
                                   primitives))
; Four possible evaluation types :
; data ExprType = Int |
;                 Bool |
;                 Proc [ExprType] ExprType |
;                 Pair ExprType ExprType 
; type Environment = Symbol -> ExprType
; type Interpreter = String -> Environment -> ExprType
(define (environment? env)
  (procedure? env))
; type Param = Symbol
; data ProcVal = Environment * [Param] * Term
(define-datatype proc-val proc-val?
  (object-procedure ; procedure in the object language
   (env environment?)
   (params pair?)
   (body term?))
  (meta-procedure ; procedure in the meta language
   (internal procedure?)))
(define (value-of exp env)
  (define (extract-nary op)
    (define (cond-evaluator conds env)
      (define (extract-predicate cond-cell)
        (list-ref cond-cell 0))
      (define (extract-expression cond-cell)
        (list-ref cond-cell 1))
      (if (value-of (extract-predicate (car conds)) env)
          (value-of (extract-expression (car conds)) env)
          (cond-evaluator (cdr conds) env)))
    (cond
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
  (define (value-of-proc params body env)
    (object-procedure env params body))
  (define (value-of-call rator rands env)
    (define (apply-procedure proc args)
      (cases proc-val proc
        (object-procedure (env params body)
                          (value-of body (extend-env-with-key-value-lists env params args)))
        (meta-procedure (internal-proc) (internal-proc env args))))
    (apply-procedure (value-of rator env) (map (lambda (arg) (value-of arg env))
                                               rands)))
  (cases program exp
    (int-exp (val) val)
    (bool-exp (val) val)
    (var-exp (var) (apply-env env var))
    (nary-exp (op args) ((extract-nary op) args env))
    (if-exp (cond then else)
            (if (value-of cond env)
                (value-of then env)
                (value-of else env)))
    (proc-exp (param body) (value-of-proc param body env))
    (call-exp (rator rand) (value-of-call rator rand env))
    (let*-exp (varlist body)
              (value-of body (extend-env-varlist-let* varlist env)))
    (let-exp (varlist body)
             (value-of body (extend-env-varlist-let varlist env)))))
(define (run program)
  (value-of (parse-tree program) (init-env)))