#lang eopl
(require "syntax-tree.scm")
(define (parse-tree tree)
  (define (parse-pair tree)
    (define (nullary-op? head)
      (eqv? head 'emptylist))
    (define (extract-nullary head)
      head)
    (define (binary-op? head)
      (or (eqv? head '+)
          (eqv? head '-)
          (eqv? head '*)
          (eqv? head 'equals?)
          (eqv? head 'greater?)
          (eqv? head 'less?)
          (eqv? head 'cons)))
    (define (extract-binary head)
      head)
    (define (unary-op? head)
      (or
       (eqv? head 'zero?)
       (eqv? head 'negate)
       (eqv? head 'car)
       (eqv? head 'cdr)
       (eqv? head 'null?)))
    (define (extract-unary head)
      head)
    (define (nary-op? head)
      (or
       (eqv? head 'list)
       (eqv? head 'cond)))
    (define (extract-nary head)
      head)
    (define (extract-nary-mapper head)
      (define (parse-cond tree)
        (list (parse-tree (list-ref tree 0))
              (parse-tree (list-ref tree 1))))
      (cond
        ((eqv? head 'list) parse-tree)
        ((eqv? head 'cond) parse-cond)))
    (define (parse-let-form constructor tree)
      (define (extract-var-list tree)
        (define (parse-var-assoc cell)
          (list (car cell)
                (parse-tree (list-ref cell 1))))
        (map parse-var-assoc (list-ref tree 1)))
      (define (extract-let-body tree)
        (list-ref tree 2))
      (constructor (extract-var-list tree)
                   (parse-tree (extract-let-body tree))))
    (define (parse-let tree)
      (parse-let-form let-exp tree))
    (define (parse-let* tree)
      (parse-let-form let*-exp tree))
    (define (proc? head)
      (eqv? head 'lambda))
    (define (parse-proc-variables tree)
      (map parse-tree (list-ref tree 1)))
    (define (parse-proc-body tree)
      (parse-tree (list-ref tree 2)))
    ; any other pair is to be interpreted as a procedure call
    ; this seems like a bad idea but let's wait and watch
    (define (call? head)
      #t)
    (define (parse-call-rator tree)
      (parse-tree (list-ref tree 0)))
    (define (parse-call-rands tree)
      (map parse-tree (cdr tree)))
    (let ((head (car tree)))
      (cond
        ((binary-op? head) (binop-exp (extract-binary head) (parse-tree (list-ref tree 1))
                                      (parse-tree (list-ref tree 2))))
        ((unary-op? head) (unary-op-exp (extract-unary head) (parse-tree (list-ref tree 1))))
        ((nary-op? head) (nary-exp (extract-nary head) (map (extract-nary-mapper head) (cdr tree))))
        ((eqv? 'if head) (if-exp (parse-tree (list-ref tree 1))
                                 (parse-tree (list-ref tree 2))
                                 (parse-tree (list-ref tree 3))))
        ((eqv? 'let head) (parse-let tree))
        ((eqv? 'let* head) (parse-let* tree))
        ((nullary-op? head) (nullary-op-exp (extract-nullary head)))
        ((proc? head) (proc-exp (parse-proc-variables tree) (parse-proc-body tree)))
        ((call? head) (call-exp (parse-call-rator tree) (parse-call-rands tree))))))
  (cond
    ((null? tree) (eopl:error 'parse-tree "Empty string"))
    ((pair? tree) (parse-pair tree))
    ((number? tree) (int-exp tree))
    ((or (eqv? 'true tree)
         (eqv? 'else tree)) (bool-exp #t))
    ((eqv? 'false tree) (bool-exp #f))
    ((symbol? tree) (var-exp tree))
    (else (int-exp tree))))
(provide parse-tree)
