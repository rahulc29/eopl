#lang eopl
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
