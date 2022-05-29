#lang eopl
; type DiffTree = (one) | (diff DiffTree DiffTree)
; zero :: DiffTree
(define (zero)
  '(diff (one) (one)))
; (diff DiffTree DiffTree) -> DiffTree
(define (left-subtree a)
  (list-ref a 1))
; (diff DiffTree DiffTree) -> DiffTree
(define (right-subtree a)
  (list-ref a 2))
; diff-eval :: DiffTree -> Int
(define (diff-eval a)
  (define (one? a)
    (eqv? 'one (car a)))
  (cond
    ((one? a) 1)
    (else (- (diff-eval (left-subtree a))
             (diff-eval (right-subtree a))))))
; diff-equals? :: DiffTree -> DiffTree -> Bool
(define (diff-equals? a b)
  (= (diff-eval a)
     (diff-eval b)))
; is-zero? :: DiffTree -> Bool
(define (is-zero? a)
  (diff-equals? (left-subtree a)
                (right-subtree a)))
; minus-one :: DiffTree
(define (minus-one)
  '(diff (diff one one) one))
; successor :: DiffTree -> DiffTree
(define (successor a)
  (list 'diff a (minus-one)))
; predecessor :: DiffTree -> DiffTree
(define (predecessor a)
  (list 'diff a 'one))
; diff-tree-plus :: DiffTree -> DiffTree -> DiffTree
(define (diff-tree-plus a b)
  (list 'diff a (list 'diff (zero) b)))