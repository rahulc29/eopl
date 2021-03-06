#lang eopl
(define (number->tree label num)
  (list label num '() '()))
(define (make-root num)
  (number->tree 'root num))
(define (make-left-child num)
  (number->tree 'left num))
(define (make-right-child num)
  (number->tree 'right num))
(define (left-child tree)
  (if (null? tree)
      '()
      (list-ref tree 2)))
(define (right-child tree)
  (if (null? tree)
      '()
      (list-ref tree 3)))
(define (current-element tree)
  (list-ref tree 1))
(define (current-mode tree)
  (list-ref tree 0))
(define (leaf? tree)
  (and (not (null? tree))
       (null? (left-child tree))
       (null? (right-child tree))))
(define (directional-insert tree
                            number
                            left-leaf-gen
                            right-leaf-gen
                            left-node-gen
                            right-node-gen)
  (cond
    ((null? tree) (make-root number))
    ((leaf? tree) (list (current-mode tree)
                        (current-element tree)
                        (left-leaf-gen number)
                        (right-leaf-gen number)))
    (else (list (current-mode tree)
                (current-element tree)
                (left-node-gen tree number)
                (right-node-gen tree number)))))