In an attempt to further accelerate my descent into madness I've decided to solve EOPL for sheer fun.

EOPL consists of increasingly complicated programming languages; my task is to write interpreters for them. 

EOPL is essentially SICP on steroids but slightly differs in the topics covered so it's also a lot of fun. :D

All the languages are in increasing order of complexity. 

## Running the interpreters

Every language is implemented as a parser, possibly a desugaring layer, and an actual interpreter. The most sophisticated interpreter yet is the one for 
LETREC. To run these interpreters, you need to install Racket and evaluate the `run` procedure in the interpreter. 

For example :

```
$> racket -i letrec-interpreter.scm 
> (run '(if (zero? 5) 
            6
            8))
#(struct:int-val 8)
```

Currently no language with effects is supported - so the semantics of every program are described entirely by their evaluation in a certain initial environment. 
The code for the interpreters is relatively well documented - you are invited to browse through the code :D 

## Parsing and syntax

When working with programming languages one has two options : 

1. implement classical concrete syntax (`let x := 5; if x > 0 then foo else bar`)
2. implement Lisp concrete syntax (`(let ((x 5)) (if (> x 0) foo bar))`) 

The good thing about the second style is that it is possible to write a parser for it by hand very easily. 
For the first style, one must use either parser combinators or parser generators since handwritten parsers can be hell. 
I'm going with handwritten parsers for now so I've chosen the second style. 

# LET 

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

To actually run the interpreter execute the script `/LET/let-interpreter.scm` with Racket and execute code with the `run` procedure. You can also see the output of the parser using the the `parse-tree` procedure. 

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


# LETREC 

Letrec is the most sophisticated of them all. It allows for recursion. For a thorough discussion of how recursion is implemented, please see `docs/RECURSION.pdf`. The design doc goes into excruciating detail with regards to fixpoints and the Y-combinator. 

Recursion can be obtained in two primary ways : through fixpoint combinators : 

```scheme
((fix (lambda (fact)
                (lambda (n)
                  (if (zero? n)
                      1
                      (* n (fact (- n 1))))))) 5)
; evaluates to 120
```

This approach is cool because it allows us to use recursion without actually naming our recursive procedures! 

A more standard way is to use the `letrec` form : 

```scheme
(letrec (fact n)
          (if (zero? n)
              1
              (* n (fact (- n 1))))
          (fact 5))
; evaluates to 120
```

Observe that `letrec` is just syntactic sugar - the language really only supports recursion through the computation of fixpoints using the Y-combinator. Again, see the design doc on recursion for more details. 

The algebraic definition is warranted for this one :

```haskell
data SugaredTerm = IntExpr Integer 
                 | VarExpr Symbol
                 | BoolExpr Bool 
                 | IfThenElseExpr SugaredTerm SugaredTerm SugaredTerm 
                 | CondExpr [SugaredTerm]
                 | ProcExpr [SugaredTerm] SugaredTerm 
                 | CallExpr SugaredTerm [SugaredTerm]
                 | LetrecExpr Symbol [SugaredTerm] SugaredTerm SugaredTerm 
                 | LetExpr [(Symbol, SugaredTerm)] SugaredTerm 
                 | LetStarExpr [(Symbol, SugaredTerm)] SugaredTerm 
data DesugaredTerm = IntExpr Integer 
                   | VarExpr Symbol
                   | BoolExpr Bool  
                   | IfThenElseExpr DesugaredTerm DesugaredTerm DesugaredTerm 
                   | CondExpr [DesugaredTerm]
                   | ProcExpr [DesugaredTerm] DesugaredTerm 
                   | CallExpr DesugaredTerm [DesugaredTerm]
                   | LetExpr [(Symbol, DesugaredTerm)] DesugaredTerm 
                   | LetStarExpr [(Symbol, DesugaredTerm)] DesugaredTerm
data Value = IntVal Integer 
           | BoolVal Bool
           | ConsVal Value Value 
           | NilVal 
           | ProcVal DesugaredTerm Environment 
desugar :: SugaredTerm -> DesugaredTerm 
parse :: String -> SugaredTerm 
eval :: DesugaredTerm -> Environment -> Value 
```
