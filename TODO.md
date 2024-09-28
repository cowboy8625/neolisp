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

### not
- returns the opposite of the first argument
- arguments type: `Bool`
- example:
```lisp
(print 10)
```

### typeof
- returns the type of the first argument
- argument type: `Any`
- example:
```lisp
(typeof 10) ; -> Number
```

### cdr
- returns a list or string without the first element
- argument
   1. List
- example:
```lisp
(cdr (list 1 2 3)) ; -> (2 3)
(cdr "hello") ; -> "ello"
```

### last
- returns a last element in list
- argument
   1. List
- example:
```lisp
(last (list 1 2 3)) ; -> 3
(last "hello") ; -> "o"
```

### append
- returns a `List` with elements appended
- arguments type: `List`
- example:
```lisp
(append (list 1 2) (list 3 4)) ; -> (1 2 3 4)
```

### reverse
- returns a list with elements in reverse order
- argument type: `List`
- example:
```lisp
(reverse (list 1 2 3)) ; -> (3 2 1)
```

### nth
- returns the nth element of a list
- arguments type: `List`
- example:
```lisp
(nth (list 1 2 3) 2) ; -> 3
```

### map
- returns a list with the result of applying a function to each element of a list
- arguments
   1. `Function`
   2. `List`
- example:
```lisp
; map takes a lambda
(map (lambda (x) (+ x 1)) (list 1 2 3)) ; -> (2 3 4)

; or

(fn addone (x) (+ x 1))
(map addone (list 1 2 3)) ; -> (2 3 4)
```

### fold
- returns the result of applying a function to each element of a list
- arguments
   1. `T` starting value
   2. `Function` `(lambda (x y) ...)`
   3. `List`
- example:
```lisp
(fold 0 + (list 1 2 3)) ; -> 6

(fn add (x y) (+ x y))
(fold 0 add (list 1 2 3)) ; -> 6
```

### fold-right
- **NOT IMPLEMENTED**
- returns the result of applying a function to each element of a list in reverse order
- arguments
   1. `Function`
   2. `List`
- example:
```lisp
(fold-right + (list 1 2 3)) ; -> 6
```

### filter
- returns a list with elements that satisfy a function
- arguments
   1. `Function`
   2. `List`
- example:
```lisp
(filter (lambda (x) (> x 1)) (list 1 2 3)) ; -> (2 3)
```

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

### assert
- throws an error if the condition is false
- arguments
   1. `Boolean` condition
   2. `String` optional error message
- example:
```lisp
(assert (> 1 2) "1 is not greater than 2")
```

### sleep
- returns false
- arguments
   1. `Number` time to sleep in milliseconds
- example:
```lisp
(sleep 1000) ; -> false
```

### to-string
- return string representation in string form
- arguments
   1. `Number`, `Boolean`, `List`
- example:
```lisp
(to-string 1000) ; -> "1000"
```

### split
- return list of string split by separator
- arguments
   1. `String` separator
   2. `String` string to split
- example:
```lisp
(split " " "(+ 1 1)") ; ->  ("(+" "1" "1)")
```

### join
- NOT IMPLEMENTED
- return string joined by separator
- arguments
   1. `String` separator
   2. `List` list of string
- example:
```lisp
(join " " (list "1" "2" "3")) ; -> "1 2 3"
```

### do
- Evaluate all the elements of the list and return the final evaluated element.
- arguments
   1. `List`
- example:
```lisp
(do (print "hello")(+ 321 123)) ; -> 444
```

### number?
- return true if the argument is a number
- arguments
   1. `Any`
- example:
```lisp
(number? 10) ; -> true
(number? "10") ; -> true
(number? "abc") ; -> false
(number? '(1 2 3)) ; -> false
```

### atom?
- NOT IMPLEMENTED
- return true if the argument is an atom
- arguments
   1. `Any`
- example:
```lisp
(atom? 10) ; -> true
(atom? "10") ; -> true
(atom? "abc") ; -> true
(atom? '(1 2 3)) ; -> false
```

### slice
- return a slice of the list or string
- arguments
   1. `String` | `List`
- example:
```lisp
(slice "abc" 1 2) ; -> "b"
(slice (list 1 2 3) 1 2) ; -> (2)
```



## Adding Help message to function

To add help message to a function, simply add a string after the function parameter in the signature.
```lisp
(fn add (x y)
  "Adds two numbers"
   (+ x y))
```
