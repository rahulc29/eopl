#lang eopl
(define (bigit bignum)
  (car bignum))
(define (add-bignum n a b)
  (define (add-with-carry a b)
    ; TODO : Implement add-with-carry
    '())
  (define (add-without-carry a b)
    (define (bigit-sum a b)
      (+ (bigit a)
         (bigit b)))
    (cons (bigit-sum a b)
          (add-bignum n
                      (cdr a)
                      (cdr b))))
                      
  (cond
    ((null? a) b)
    ((null? b) a)
    ((let ((sum (+ (bigit a)
                   (bigit b))))
       (>= n sum))
     (add-with-carry a b))
    (else (add-without-carry a b))))