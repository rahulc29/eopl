#lang eopl
; in-s? :: Integer -> Bool
(define (in-s? n)
  (define (check-inductive n)
    (let ((to-check (- n 3)))
      (if (>= to-check 0)
          (in-s? to-check)
          #f)))
  (cond
    ((= 0 n) #t)
    ((check-inductive n) #t)
    (else #f)))
; list-length :: List -> Integer
; list-length = foldr (\curr rest -> 1 + rest) 0
(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))
; list-ref :: List -> Any
(define (list-ref lst n)
  (define (report-list-too-short n)
    (eopl:error 'list-ref "The list is too short"))
  (if (null? lst)
      (report-list-too-short n)
      (if (= n 0)
          (car lst)
          (list-ref (cdr lst) (- n 1)))))
; remove-first :: List -> Symbol -> List
(define (remove-first lst sym)
  (if (null? lst)
      '()
      (if (eqv? (car lst) sym)
          (cdr lst)
          (cons (car lst)
                (remove-first (cdr lst) sym)))))
; remove :: List -> Symbol -> List
(define (remove lst sym)
  (if (null? lst)
      lst
      (if (eqv? (car lst) sym)
          (remove (cdr lst) sym)
          (cons (car lst)
                (remove (cdr lst) sym)))))