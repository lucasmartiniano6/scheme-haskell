# Scheme-Haskell
Writing an interpreter for a custom programming language (~Scheme) written in Haskell. Why? Scheme is a dialect of Lisp and Haskell is cool.

![scheme](https://upload.wikimedia.org/wikipedia/commons/thumb/3/39/Lambda_lc.svg/121px-Lambda_lc.svg.png)

Nice TODO
-----
* Increment Standard Library
* Implement Hygienic macros
* Add exception handling

Usage
-----
```scheme
Lisp>> (load "stdlib.scm")
Lisp>> (define x 2)
Lisp>> (+ x 3)
5
Lisp>> (reverse '(1 2 3 4))
(4 3 2 1)
Lisp>> (sum '(1 2 3 4))
10
Lisp>> (filter even? '(1 2 3 4))
(2 4)
Lisp>> (map (curry + 2) '(1 2 3 4))
(3 4 5 6)
Lisp>> (map (curry > 3) '(1 2 3 4))
(#t #t #f #f)
Lisp>> (not (and (xor #t #f) #t))
#f
Lisp>> (define (fac x) (if (= x 1) 1 (* x (fac (- x 1)))))
Lisp>> (fac 4)
24
```
Implementation
-----
Based on:
  * https://upload.wikimedia.org/wikipedia/commons/a/aa/Write_Yourself_a_Scheme_in_48_Hours.pdf
  * https://hoogle.haskell.org/ 
