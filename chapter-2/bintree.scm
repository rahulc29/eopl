#lang eopl
(define (number->bintree num)
  (list num '() '()))
(define (current-element tree)
  (list-ref tree 0))
(define (left-child tree)
  (if (null? tree)
      '()
      (list-ref tree 1)))
(define (right-child tree)
  (if (null? tree)
      '()
      (list-ref tree 2)))
(define (leaf? tree)
  (and (null? (left-child tree))
       (null? (right-child tree))))
(define (directional-insert tree
                            num
                            left-leaf-gen
                            right-leaf-gen
                            left-node-gen
                            right-node-gen)
  (cond
    ((null? tree) (number->bintree num))
    ((leaf? tree) (list (current-element tree)
                        (left-leaf-gen num)
                        (right-leaf-gen num)))
    (else (list (current-element tree)
                (left-node-gen tree num)
                (right-node-gen tree num)))))
(define (insert-to-left tree num)
  (directional-insert tree
                      num
                      number->bintree
                      (lambda (x) '())
                      (lambda (tree num)
                        (insert-to-left (left-child tree) num))
                      (lambda (tree num)
                        (right-child tree))))
(define (insert-to-right tree num)
  (directional-insert tree
                      num
                      (lambda (x) '())
                      number->bintree
                      (lambda (tree num)
                        (left-child tree))
                      (lambda (tree num)
                        (insert-to-right (right-child tree) num))))
