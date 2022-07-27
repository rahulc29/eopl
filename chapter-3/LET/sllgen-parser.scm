#lang eopl
(define lexical-spec
  '((whitespace (whitespace) skip)
    (comments ("#" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "-" "->" "?"))) symbol)
    (operator ((or "+" "-" "*" "/" ":=" "=")) symbol)
    (number (digit (arbno digit)) number)))
(define scan
  (sllgen:make-string-scanner lexical-spec '()))
