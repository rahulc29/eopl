#lang eopl
; number-elements :: List<A> -> List<(Int, A)>
(define (number-elements lst)
  ; number-elements-from :: Int -> List<A> -> List<(Int, A)>
  (define (number-elements-from k lst)
    (if (null? lst)
        '()
        (cons (list k (car lst))
              (number-elements-from (+ k 1) (cdr lst)))))
  (number-elements-from 0 lst))
; vector-sum :: (Add a, Index i) => Vector<a, i> -> a
(define (vector-sum vec)
  ; partial-vector-sum :: (Add a, Index i) => Vector<a, i> -> i -> a
  (define (partial-vector-sum vec n)
    (if (= n 0)
        (vector-ref vec 0)
        (+ (vector-ref vec n)
           (partial-vector-sum vec (- n 1)))))
  (let ((n (vector-length vec)))
    (if (= n 0)
        0
        (partial-vector-sum vec (- n 1)))))