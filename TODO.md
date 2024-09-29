- LANGUAGE FEATURE
### let
- binds a list of values to symbols in the current scope and returns the last evaluated value
- arguments
   1. `Symbol`: name of the variable
   2. `Any`: value
- example:
```lisp
(let
   ((x 123)
   (y 321))
   (+ x y)) ; -> 444
```

- LANGUAGE FEATURE
### loop
- returns the result of last iteration
- arguments
   1. `Function`
   2. `List`
- example:
```lisp
; returns -> (2)
(let (x 0)
(loop (< x 3)
    (var x (+ x 1))))
```

- LANGUAGE FEATURE
### do
- Evaluate all the elements of the list and return the final evaluated element.
- arguments
   1. `List`
- example:
```lisp
(do (print "hello")(+ 321 123)) ; -> 444
```

- LANGUAGE FEATURE
## Adding Help message to function

To add help message to a function, simply add a string after the function parameter in the signature.
```lisp
(fn add (x y)
  "Adds two numbers"
   (+ x y))
```
