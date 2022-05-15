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
; insertion-sort/predicate :: (Any a) => pred :: (a -> a -> Bool) -> List<a> -> List<a>
; where pred <= Ord a
(define (insertion-sort/predicate pred lst)
  (define (insert lst elem)
    (if (null? lst)
        (list elem)
        (if (pred elem (car lst))
            (cons elem lst)
            (cons (car lst)
                  (insert (cdr lst) elem)))))
  (define (insert-all from to)
    (if (null? from)
        to
        (insert (insert-all (cdr from) to)
                (car from))))
  (insert-all lst '()))
; ------------------------------
; Tagged Union interface
; make-tagged :: Symbol -> Any -> (Symbol, Any)
(define (make-tagged tag data)
  (list tag data))
; data-of :: (Symbol, Any) -> Any
(define (data-of tagged)
  (cadr tagged))
; tag-of :: (Symbol, Any) -> Symbol
(define (tag-of tagged)
  (car tagged))
; ------------------------------

; Binary Tree interface
; (Any a) =>
; leaf :: a -> Tree a
; interior-node :: a -> Tree a -> Tree a -> Tree a
; leaf constructor
(define (leaf content)
  (make-tagged 'leaf content))
; interior node constructor
(define (interior-node content lchild rchild)
  (make-tagged 'node (list content lchild rchild)))
; tag validator
; tag-validate :: Tree<a> -> Symbol -> Symbol -> Bool | Nothing
(define (tag-validate node expected errname)
  (if (null? node)
      (eopl:error errname "Expected non-null value")
      (eqv? expected (tag-of node))))
; utility error procedure
(define (invalid-tree-error name)
  (eopl:error name "Expected valid binary tree"))
; leaf? validator
(define (leaf? node)
  (tag-validate node 'leaf 'leaf?))
; node? validator
(define (node? node)
  (tag-validate node 'node 'node?))
; contents-of :: Tree<a> -> a &
;                _ -> Nothing
(define (contents-of node)
  (define (contents-of-leaf node)
    (data-of node))
  (define (contents-of-interior-node node)
    (define (contents-of-list node-data)
      (car node-data))
    (contents-of-list (data-of node)))
  (cond
    ((null? node) eopl:error 'contents-of "Cannot compute contents of empty node")
    ((leaf? node) (contents-of-leaf node))
    ((node? node) (contents-of-interior-node node))
    (else (eopl:error 'contents-of "Expected valid binary tree"))))
; left child selector
(define (left-child interior)
  (list-ref (data-of interior) 1))
; right child selector
(define (right-child interior)
  (list-ref (data-of interior) 2))
; functor for trees
; tree-map :: (a -> b) -> Tree<a> -> Tree<b>
; takes a function of the form (a -> b)
; can be interpreted as returning a new function of the form (Tree<a> -> Tree<b>)
; therefore a functor
(define (tree-map f tree)
  (cond
    ((null? tree) '())
    ((leaf? tree) (leaf (f (contents-of tree))))
    ((node? tree) (interior-node (f (contents-of tree))
                                 (tree-map f (left-child tree))
                                 (tree-map f (right-child tree))))
    (else (invalid-tree-error 'tree-map))))
; marks all leaves with their depth from nearest red root
; mark-leaves-with-red-depth :: Tree<a> -> Tree<(a, Int)>
(define (mark-leaves-with-red-depth tree)
  (define (mark-leaves-node tree k) ; k = current depth, should've used a better name
    (define (red-node? tree)
      (eqv? 'red (contents-of tree)))
    (define (mark-child tree k)
      (define (mark-node tree k)
        (cond
          ((red-node? tree) (mark-leaves-node tree 1))
          (else (mark-leaves-node tree (+ k 1)))))
      (cond
        ((null? tree) '())
        ((leaf? tree) (leaf (list (contents-of tree) k)))
        ((node? tree) (mark-node tree k))
        (else (invalid-tree-error 'mark-leaves-with-red-depth))))
    (let ((left (left-child tree))
          (right (right-child tree)))
      (interior-node (contents-of tree)
                     (mark-child left k)
                     (mark-child right k))))
      
  (cond
    ((null? tree) '())
    ((leaf? tree) (leaf (list (contents-of tree) 0)))
    ((node? tree) (mark-leaves-node tree 1))
    (else (invalid-tree-error 'mark-leaves-with-red-depth))))
; Numbers the leaves starting from 0
; The implementation essentially simulates the State monad
; Admittedly, this is not a pretty design
; But I couldn't think of anything better :/
; By changing the last selector from extract-tree to extract-k
; we can get the number of non-empty leaves in the tree tho :D
; This algorithm is essentially DFS but we do not need to maintain a visited array
; since the structure of the tree ensures it
(define (number-leaves tree)
  (define (number-leaf tree k)
    (list (leaf k) (+ k 1)))
  (define (extract-k res)
    (list-ref res 1))
  (define (extract-tree res)
    (list-ref res 0))
  (define (number-node tree k)
    (let* ((left-res (number-leaves-impl
                      (left-child tree)
                      k))
           (right-res (number-leaves-impl
                       (right-child tree)
                       (extract-k left-res))))
      (list (interior-node (contents-of tree)
                           (extract-tree left-res)
                           (extract-tree right-res))
            (extract-k right-res))))
  (define (number-leaves-impl tree k)
    (cond
      ((null? tree) (list tree k))
      ((leaf? tree) (number-leaf tree k))
      ((node? tree) (number-node tree k))
      (else (invalid-tree-error 'number-leaves))))
  (extract-tree (number-leaves-impl tree 0)))

; Binary Search Tree Interface :
; All S-expressions over an alphabet of comparables
; that satisfy the BST constraint :
; t :: Ord 
; (a b c)
; a :: t
; b :: Maybe<Tree<Int>>
; c :: Maybe<Tree<Int>>
; forall a (Some b) (Some c). b < a &&  a <= c

; returns key of BST
; key :: BST<t> -> t
(define (key bst)
  (if (pair? bst)
      (car bst)
      bst))
; auxillary procedure
; monadically returns the list-ref at a given index
; non-null-ref :: Maybe<List<a>> -> Maybe<a>
(define (non-null-ref lst idx)
  (if (null? lst)
      '()
      (list-ref lst idx)))
; (maybe) returns the left BST of a BST
; left-bst :: BST<t> -> Maybe<BST<t>>
(define (left-bst bst)
  (non-null-ref bst 1))
; (maybe) returns the right BST of a BST
; right-bst :: BST<t> -> Maybe<BST<t>>
(define (right-bst bst)
  (non-null-ref bst 2))
; Returns the path to go from root to 'n' in the given BST
; returns 'not-found if the value is not found
; returns 'not-found if querying in empty BST
; path :: (Num, Ord t) => t -> BST<t> -> Symbol | List<Symbol>
(define (path n bst)
  (define (in-branch? cmp bst n)
    (cmp n (key bst)))
  (define (in-left? bst n)
    (in-branch? < bst n))
  (define (in-right? bst n)
    (in-branch? >= bst n))
  (cond
    ((null? bst) 'not-found)
    ((= n (key bst)) '())
    ((in-left? bst n) (cons 'left (path n (left-bst bst))))
    ((in-right? bst n) (cons 'right (path n (right-bst bst))))
    (else eopl:error 'path "Illegal operation")))