#lang eopl
(require (prefix-in sugared: "sugared-syntax-tree.scm"))
(require "desugared-syntax-tree.scm")
; The desugar procedure takes a syntax tree
; and returns a new syntax tree in which
; everything has been desugared
; This means we will have two different
; syntax tree datatypes (sugared and desugared)
; A frontend parser that parses everything into
; a sugared syntax tree
; This middle-end parser that converts everything
; into the a desugared simplified syntax tree
; The interpreter then, consumes the desugared
; syntax tree and evaluates it in the initial
; environment.
; Adding a desugaring layer makes significant
; portions of our life extremely convenient
; Most importantly, definitions can be
; implemented as syntactic sugar over let bindings
; This _might_ breakdown when we introduce types
; but ok.
; TODO : Implement desugaring and desugar datatypes.
(define (pass-let-vars vars pass)
  (define (pass-var var)
    (pass (list-ref var 1)))
  (map pass-var vars))
(define (desugar-letrec name args proc-body expr-body)
  (define (transform-proc-body proc-body)
    (call-exp (var-exp 'fix)
              (list (proc-exp (list name)
                        (proc-exp args (desugar proc-body))))))
  (let-exp (list (list name (transform-proc-body proc-body))) (desugar expr-body)))
(define (desugar syntax)
  (define (desugar-let-vars vars)
    (define (desugar-var var)
      (desugar (list-ref var 1)))
    (map desugar-var vars))
  (cases sugared:sugared-tree syntax
    (sugared:int-exp (val) (int-exp val))
    (sugared:bool-exp (val) (bool-exp val))
    (sugared:var-exp (val) (var-exp val))
    (sugared:if-exp (pred then-clause else-clause)
                    (if-exp (desugar pred) (desugar then-clause) (desugar else-clause)))
    (sugared:nary-exp (op opands) (nary-exp op (desugar opands)))
    (sugared:proc-exp (params body) (proc-exp params (desugar body)))
    (sugared:call-exp (rator rands) (call-exp (desugar rator) (map desugar rands)))
    (sugared:let*-exp (vars body) (let*-exp (desugar-let-vars vars) (desugar body)))
    (sugared:let-exp (vars body) (let-exp (desugar-let-vars vars) (desugar body)))
    (sugared:letrec-exp (name args proc-body expr-body) (desugar-letrec name args proc-body expr-body))))
 (provide desugar)
