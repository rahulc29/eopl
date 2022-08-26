#lang eopl
(require "sugared-syntax-tree.scm")
(require (prefix-in desugared: "desugared-syntax-tree.scm"))
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
(define (desugar syntax)
  (define (desugar-let-vars vars)
    (define (desugar-var var)
      (desugar (list-ref var 1)))
    (map desugar-var vars))
  (cases sugared-tree syntax
    (int-exp (val) (desugared:int-exp val))
    (bool-exp (val) (desugared:bool-exp val))
    (var-exp (val) (desugared:var-exp val))
    (if-exp (pred then-clause else-clause)
            (desugared:if-exp (desugar pred) (desugar then-clause) (desugar else-clause)))
    (nary-exp (op opands) (desugared:nary-exp op (desugar opands)))
    (proc-exp (params body) (desugared:proc-exp params (desugar body)))
    (call-exp (rator rands) (desugared:call-exp (desugar rator) (map desugar rands)))
    (let*-exp (vars body) (desugared:let*-exp (desugar-let-vars vars) (desugar body)))
    (let-exp (vars body) (desugared:let-exp (desugar-let-vars vars) (desugar body)))
    ; TODO : Desugar letrec
    (letrec-exp (name args proc-body expr-body) '())))
    
