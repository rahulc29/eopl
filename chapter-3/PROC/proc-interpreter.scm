#lang eopl
(require "procedural-environment.scm")
(require "proc-parser.scm")
(require "syntax-tree.scm")
; Utility procedures to manipulate environments
; These can be rewritten as left folds and that would be more elegant
; but I'm lazy :P
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
; ----------- DATA STRUCTURES ----------
; These data structures are written a Haskell syntax
; since Haskell has arguably the most elegant syntax
; to define types algebraically.
; We have four possible expressible types :
; data ExprType = Integer |
;                 Bool |
;                 Proc [ExprType] ExprType |
;                 Pair ExprType ExprType
; type Environment = Symbol -> ExprType
; type Interpreter = String -> Environment -> ExprType
(define (expr-val? x)
  (or (sexp-val? x)
      (int-val? x)
      (bool-val? x)
      (proc-val? x)))
(define-datatype sexp-val sexp-val?
  (empty-sexp)
  (cons-sexp (head expr-val?)
             (tail expr-val?)))
(define (environment? env)
  (procedure? env))
; type Param = Symbol
; data IntegerVal = Integer
(define-datatype integer-val int-val?
  (int-val
   (val integer?)))
; data BooleanVal = Boolean
(define-datatype boolean-val bool-val?
  (bool-val
   (val (lambda (x) (or (eq? x #t) (eq? x #f))))))
; data ProcVal = Environment * [Param] * Term
(define-datatype proc-val proc-val?
  (object-procedure ; procedure in the object language
   (env environment?)
   (params pair?)
   (body term?))
  (meta-procedure ; procedure in the meta language
   (internal procedure?)))
; -------- WRAPPERS and UNWRAPPERS -----------
; Procedures that convert from the object language's type system
; to the meta langauge's type system (Scheme) and vice versa
(define (int-val-wrapper int)
  (int-val int))
(define (int-val-unwrapper exp)
  (cases integer-val exp
    (int-val (int) int)))
(define (bool-val-wrapper exp)
  (bool-val exp))
(define (bool-val-unwrapper exp)
  (cases boolean-val exp
    (bool-val (val) val)))
(define (sexp-val-unwrapper exp)
  (cases sexp-val exp
    (empty-sexp () '())
    (cons-sexp (head tail)
               (cons head tail))))
(define (sexp-val-wrapper exp)
  (if (null? exp)
      (empty-sexp)
      (cons-sexp (car exp)
                 (cdr exp))))
; --------- INITIAL ENVIRONMENT -------------
; Our interpreter starts in an initial environment in which
; bindings are given for primitive procedures
; This procedure returns the initial environment and sets it up
(define (init-env)
  ; -------- CONVERTERS --------
  ; These procedures convert lifted procedures of the meta language
  ; to procedures in the object language that can be consumed
  ; directly by the interpreter
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
    (define (internal-sort-checker op curr args acc conn)
      (if (null? args)
          acc
          (internal-sort-checker op
                                 (car args)
                                 (cdr args)
                                 (conn acc (op curr (car args)))
                                 conn)))
    (meta-procedure
     (lambda (env args)
       (if (null? args)
           (bool-val #t)
           (internal-sort-checker op
                                  (car args)
                                  (cdr args)
                                  (bool-val #t)
                                  (lift-binary (lambda (x y) (and x y))
                                               bool-val-unwrapper
                                               bool-val-unwrapper
                                               bool-val-wrapper))))))
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
  ; --------------- LIFTERS ---------------
  ; Since we use a dynamically checked type system (sort of, it is a little crude
  ; to call what we have a "type system" but there's no other word to describe it)
  ; we need to take primitive Scheme procedures and turn them into procedures
  ; that can be invoked from the object language
  ; To do this, we simply apply a wrapper and an unwrapper.
  ; f' = wrap . f . unwrap
  ; lift f wrap unwrap = \x -> wrap $ f $ unwrap x
  ; Other arities work similarly by taking more unwrappers
  (define (lift-unary op unwrapper wrapper)
    (lambda (x)
      (wrapper
       (op (unwrapper x)))))
  (define (lift-binary op unwrapper-1 unwrapper-2 wrapper)
    (lambda (x y)
      (wrapper (op (unwrapper-1 x)
                   (unwrapper-2 y)))))
  (define (lift-arithmetic op)
    (lift-binary op int-val-unwrapper int-val-unwrapper int-val-wrapper))
  (define (lift-comparator comp)
    (lift-binary comp int-val-unwrapper int-val-unwrapper bool-val-wrapper))
  ; ------ PRIMITIVES -------------
  ; This list defines all the primitives in the initial environment
  ; type Primitive = Symbol * ExprType
  ; Primitives are defined as the Cartesian Product of Symbol
  ; and an expressed value. The expressed value is then bound
  ; to the symbol in the initial environment
  (define primitives
    (list (cons '+ (make-foldr (lift-arithmetic +) (int-val 0)))
          (cons '* (make-foldr (lift-arithmetic *) (int-val 1)))
          (cons '- (make-foldr (lift-arithmetic -) (int-val 0)))
          (cons 'cons (make-binary cons-sexp))
          (cons '= (make-binary (lift-comparator =)))
          (cons 'car (make-unary car))
          (cons 'cdr (make-unary cdr))
          (cons 'negate (make-unary (lift-unary (lambda (x) (- x)) int-val-unwrapper int-val-wrapper)))
          (cons 'zero? (make-unary (lift-unary zero? int-val-unwrapper bool-val-wrapper)))
          (cons 'null? (make-unary (lift-unary null? sexp-val-unwrapper bool-val-wrapper)))
          (cons 'list (make-foldr cons-sexp (empty-sexp)))
          (cons 'emptylist (make-meta (lambda (x) (empty-sexp))))
          (cons '< (make-sort-checker (lift-comparator <)))
          (cons '> (make-sort-checker (lift-comparator >)))
          (cons '>= (make-sort-checker (lift-comparator <=)))
          (cons '<= (make-sort-checker (lift-comparator >=)))))
  ; The initial environment is made by extending the empty environment
  ; with the primitives.
  (extend-env-with-key-value-pairs (empty-env)
                                   primitives))
; ------------ EVALUATOR --------------
; The heart of the interpreter
; The value-of an expression in an environment
; is defined by this procedure recursively.
; If the expression is well-formed then the evaluation
; is guranteed to terminate in the sense that
; it will reach the base case of evaluation.
; The evaluation of these base cases may itself be recursive
; in the object language. If the recursion in the object
; language does not terminate then a non-terminal recursion
; is induced in the meta language.
; Mathematically, this is NOT a total function.
; Indeed, the interpretation of any Turing complete
; language must involve non-terminating computations
; and therefore partiality.
(define (value-of exp env)
  ; --------- UTILITIES ------------
  ; These procedures give back utility lambdas to be consumed by
  ; the interpreter
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
  (define (true? bool)
    (eq? #t (bool-val-unwrapper bool)))
  ; The consumption of the abstract syntax tree begins here :D
  ; This is where magic happens ;)
  (cases program exp
    (int-exp (val) (int-val val))
    (bool-exp (val) (bool-val val))
    (var-exp (var) (apply-env env var))
    (nary-exp (op args) ((extract-nary op) args env))
    (if-exp (condition then-clause else-clause)
            (if (true? (value-of condition env))
                (value-of then-clause env)
                (value-of else-clause env)))
    (proc-exp (param body) (value-of-proc param body env))
    (call-exp (rator rand) (value-of-call rator rand env))
    (let*-exp (varlist body)
              (value-of body (extend-env-varlist-let* varlist env)))
    (let-exp (varlist body)
             (value-of body (extend-env-varlist-let varlist env)))))
; To run a program is to evaluate it in the initial environment
(define (run program)
  (value-of (parse-tree program) (init-env)))