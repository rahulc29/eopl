In an attempt to further accelerate my descent into madness I've decided to solve EOPL for sheer fun.

EOPL consists of increasingly complicated programming languages; my task is to write interpreters for them. 

I've currently only implemented the first language, LET, and the interpreter can be found in `/chapter-3/LET/`. 

EOPL is essentially SICP on steroids but slightly differs in the topics covered so it's also a lot of fun. :D

## Parsing and syntax

When working in Lisps one has two options :

1. implement classical concrete syntax (`let x := 5; if x > 0 then foo else bar`)
2. implement Lisp concrete syntax (`(let ((x 5)) (if (> x 0) foo bar))`) 

The good thing about the second style is that it is possible to write a parser for it by hand very easily. For the first style, one must use either parser combinators or parser generators since handwritten parsers can be hell. I'm going with handwritten parsers for now so I've chosen the first style. 

## LET 

LET allows integer arithmetic, if-else, binding of variables and untyped linked lists. 

Here is the algebraic definition of the abstract syntax : 

```haskell
data Term = IntConst Integer | 
            BoolConst Boolean | 
            Variable Symbol |
            NullaryOperator Symbol |
            UnaryOperator Symbol Term |
            BinaryOperator Symbol Symbol Term |
            IfThenElse Term Term Term |
            NaryOperator Symbol [Term] |
            LetExpr [(Symbol, Term)] Term |
            LetStarExpr [(Symbol, Term)] Term 
type Environment = Symbol -> Term 
type Interpreter = Term -> Environment -> Term 
```

To actually run the interpreter execute the script `/chapter-3/LET/let-interpreter.scm` with Racket and execute code with the `run` procedure. You can also see the output of the parser using the the `parse-tree` procedure. 

Example programs : 

```scheme 
(let ((x 30))
          (let* ((x (- x 1))
                 (y (- x 2)))
            (- x y)))
; evaluates to 2
```

```scheme
(let* ((x 30)
       (y (list x x)))
          (if (zero? x)
              y
              (* 2 x)))
; evaluates to 60
```

A more complicated example :

```scheme
(let* ((x 11)
       (y (emptylist))
       (z (+ x 17)))
          (car (if (equals? x z)
                   (cons z y)
                   (cons (cons z y) y))))
; (28)
```

# PROC 

PROC allows us to also have procedure and procedure-calls. 
Essentially all forms remain the same but procedures also become first-class values. 

Currying : 
```scheme
(let (curry (lambda (x) 
             (lambda (y) 
              (+ x y))))
 ((curry 3) 4))
; evaluates to 7
```

Recursion : 
```scheme
(let (fact (lambda (n self) 
            (if (zero? n)
             1
             (* n (self (- n 1) self)))))
 (fact 5 fact))
; 120 
```

Observe that recursion without explicitly passing the function itself as a parameter would require the Y combinator and can be done. 
The existence of the Y-combinator also shows that PROC is Turing complete. 
