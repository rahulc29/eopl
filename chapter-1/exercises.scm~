#lang eopl
; duple :: (Any a) => Int -> a -> List<a>
(define (duple n x)
  (if (= n 0)
      '()
      (cons x
            (duple (- n 1) x))))
; invert :: (Any a b) => List<(a, b)> -> List<(b, a)>
(define (invert lst)
  ; first :: (Any a b) => (a, b) -> a
  (define (first cell)
    (car cell))
  ; second :: (Any a b) => (a, b) -> b
  (define (second cell)
    (cadr cell))
  ; invert-comp :: (Any a b) => (a, b) -> (b, a)
  (define (invert-comp comp)
    (list (second comp)
          (first comp)))
  (map invert-comp lst))
; down :: (Any a) => List<a> -> List<(a)>
(define (down lst)
  ; down-element :: (Any a) => a -> (a)
  (define (down-element element)
    (list element))
  (map down-element lst))
; swapper :: Symbol -> Symbol -> Slist -> Slist
(define (swapper s1 s2 slist)
  ; symbol-swapper :: Symbol -> Symbol -> Symbol -> Symbol
  (define (symbol-swapper s1 s2 sym)
    (cond
      ((eqv? s1 sym) s2)
      ((eqv? s2 sym) s1)
      (else sym)))
  ; sexp-swapper :: Symbol -> Symbol -> Sexp -> Sexp
  (define (sexp-swapper s1 s2 sexp)
    (if (symbol? sexp)
        (symbol-swapper s1 s2 sexp)
        (slist-swapper s1 s2 sexp)))
  ; slist-swapper :: Symbol -> Symbol -> Slist -> Slist
  (define (slist-swapper s1 s2 slist)
    (map (lambda (sexp) (sexp-swapper s1 s2 sexp))
         slist))
  (slist-swapper s1 s2 slist))
; list-set :: (Any a b) => List<a> -> Int -> b -> List<a|b>
(define (list-set lst n x)
  (if (null? lst) '()
      (if (= n 0)
          (cons x (cdr lst))
          (cons (car lst)
                (list-set (cdr lst)
                          (- n 1)
                          x)))))
; count-occurences :: Symbol -> Slist -> Int
(define (count-occurences s slist)
  ; count-occurences-symbol :: Symbol -> Symbol -> Int
  (define (count-occurences-symbol s symbol)
    (if (eqv? s symbol)
        1
        0))
  ; count-occurences-sexp :: Symbol -> Sexp -> Int
  (define (count-occurences-sexp s sexp)
    (if (symbol? sexp)
        (count-occurences-symbol s sexp)
        (count-occurences s sexp)))
  ; count-occurences-slist :: Symbol -> Slist -> Int
  (define (count-occurences-slist s slist)
    (if (null? slist)
        0
        (+ (count-occurences-sexp s (car slist))
           (count-occurences s (cdr slist)))))
  (count-occurences-slist s slist))
(define (foldr lst id f)
  (if (null? lst)
      id
      (f (car lst)
         (foldr (cdr lst) id f))))
; product :: List<Symbol> -> List<Symbol> -> List<(Symbol, Symbol)>
(define (product s1 s2)
  (define (product-singleton elem lst)
    (map (lambda (list-elem) (list elem list-elem))
         lst))
  (foldr (map (lambda (elem)
                (product-singleton elem s2))
              s1)
         '()
         append))
; list-index :: (Any a) => (a -> Bool) -> List<a> -> Int
(define (list-index pred lst)
  (define (list-index-partial pred lst k)
    (if (null? lst)
        #f
        (if (pred (car lst))
            k
            (list-index-partial pred (cdr lst) (+ k 1)))))
  (list-index-partial pred lst 0))
; up :: List -> List
(define (up lst)
  (define (up-non-empty lst)
    (let ((fst (car lst))
          (rest (up (cdr lst))))
      (if (list? fst)
          (append fst
                  rest)
          (cons fst rest))))
  (if (null? lst)
      '()
      (up-non-empty lst)))
; flatten :: Slist -> List<Symbol>
(define (flatten slist)
  ; flatten-sexp :: Sexp -> List<Symbol>
  (define (flatten-sexp sexp)
    (if (symbol? sexp)
        (list sexp)
        (flatten sexp)))
  (if (null? slist)
      '()
      (append (flatten-sexp (car slist))
              (flatten (cdr slist)))))
; merge :: (Ord a, Size n m) => List<a, n> -> List<a, n> -> List<a, n + m>
(define (merge loi1 loi2)
  (cond
    ((null? loi1) loi2)
    ((null? loi2) loi1)
    ((< (car loi1) (car loi2))
     (cons (car loi1) (merge (cdr loi1) loi2)))
    (else
     (cons (car loi2) (merge loi1 (cdr loi2))))))
