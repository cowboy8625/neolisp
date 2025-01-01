# NeoLisp Docs

## Functions

### fn
- returns a `Function`
- arguments
   1. `Symbol`: name of the function
   2. `List`: arguments
   3. `expressions`: body
- example:
```lisp
(fn add (x y) (+ x y))
(add 10 20) ; -> 30
(fn id (x) x)
(id 10) ; -> 10
(fn any-number-of-expressions-in-body (x y z) (var a 10)(+ a x y z))
```

### lambda
- returns a `Function`
- arguments
   1. `List`: arguments
   2. `expression`: body
- example:
```lisp
(lambda (x y) (+ x y))
(10 20) ; -> 30
(fn id (x) x)
(id 10) ; -> 10
(fn any-number-of-expressions-in-body (x y z) (var a 10)(+ a x y z))
```

### var
- defines a variable
- arguments
   1. `Symbol`: name of the variable
   2. `Any`: value
- example:
```lisp
(var x 123)
(var y 321)
(+ 123 321) ; -> 444
```

### set
- sets the value of a variable that has been already defined with `var`
- arguments
   1. `Symbol`: name of the variable
   2. `Any`: value
- example:
```lisp
(var x 123)
(var y 321)
(+ 123 321) ; -> 444
```

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

### if
- returns the first argument if the condition is true
- arguments
   1. `Bool`: condition
   2. `Any`: true
   3. `Any`: false
- example:
```lisp
(if true 10 20) ; -> 10
(if false 10 20) ; -> 20
```

### +
- returns a sum of the arguments
- arguments type: `Number`
- example:
```lisp
(+ 10 20) ; -> 30
```

### -
- returns the difference of the arguments
- arguments type: `Number`
- example:
```lisp
(- 20 10) ; -> 10
```

### *
- returns the product of the arguments
- arguments type: `Number`
- example:
```lisp
(* 10 20) ; -> 200
```

### /
- returns the quotient of the arguments
- arguments type: `Number`
- example:
```lisp
(/ 20 10) ; -> 2
```

### mod
- returns the remainder of the arguments
- arguments type: `Number`
- example:
```lisp
(mod 10 2) ; -> 0
```

### =
- returns true if all the arguments are equal to the first
- arguments type: `Number`
- example:
```lisp
(= 10 10) ; -> true
```

### >
- returns true if the first argument is greater than the rest
- arguments type: `Number`
- example:
```lisp
(> 10 10) ; -> false
```

### <
- returns true if the first argument is less than the rest
- arguments type: `Number`
- example:
```lisp
(< 10 10) ; -> false
```

### >=
- returns true if the first argument is greater than or equal to the rest
- arguments type: `Number`
- example:
```lisp
(>= 10 10) ; -> true
```

### <=
- returns true if the first argument is less than or equal to the rest
- arguments type: `Number`
- example:
```lisp
(<= 10 10)
```

### and
- returns `Nil`
- arguments type: `Bool`
- example:
```lisp
(print 10)
```
### or
- returns true if any of the arguments are true
- arguments type: `Bool`
- example:
```lisp
(print 10)
```

### not
- returns the opposite of the first argument
- arguments type: `Bool`
- example:
```lisp
(print 10)
```

### print
- returns the first argument
- arguments type: `Any`
- example:
```lisp
(print 10)
```

### type?
- returns the type of the first argument
- argument type: `Any`
- example:
```lisp
(fn add (x y) (+ x y))
(type? add) ; -> Function
(type? 10) ; -> f32
```

### help
- **Currently Help does not work on all built-in functions**
- returns this message as a `String`
- argument type: `Function`
- example:
```lisp
(help +)
; - returns a sum of the arguments
; - arguments type: `Number`
; - example:
; ```lisp
; (+ 10 20) ; -> 30
; -------
; prints the help message of the function
```

### list
- returns a `List` of elements
- arguments type: `Any`
- example:
```lisp
(list 1 2 3) ; -> (1 2 3)
```

### cons
- returns a list with an element added to the beginning
- arguments
   1. item to add
   2. List
- example:
```lisp
(cons 1 (list 2 3)) ; -> (1 2 3)
```

### car
- returns the first element of a list or string
- argument
   1. `List` | `String`
- example:
```lisp
(car (list 1 2 3)) ; -> 1
(car "hello") ; -> "h"
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

### length
- returns the length of a list
- argument type: `List`
- example:
```lisp
(length (list 1 2 3)) ; -> 3
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
    (set x (+ x 1))))
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

### assert-eq
- throws an error if the arguments are not equal
- arguments
   1. `Any` first argument
   2. All other arguments need to be equal to the first
   3. `String` optional error message in last argument
- example:
```lisp
(assert-eq
   :expected 1
   :actual 2
   :description "1 is not equal than 2")
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

### ffi-bind
- creates a binding to a dll/so file
- example:
```lisp
(ffi-bind
   :libname "./libname.so"
   :symbol "function_name_in_lib"
   :fn name-you-want-to-bind-to
   :args (int float string)
   :return int)

(ffi-bind
   :libname "./libname.so"
   :symbol "function_name_in_lib"
   :struct Foo
   :fields (:baz int :bar int)


(fn main ()
   (var foo (Foo :baz 10 :bar 20))
   (name-you-want-to-bind-to 10 20.0 "hello"))
```

### struct
- creates a struct type with getters and setters
- example:
```lisp
(struct Person :name string :age int)

(fn main ()
   (var person (Person :name "John" :age 20))
   (Person:set person :name "John Doe")
   (var name (Person:get person :name)))
```


## Adding Help message to function

To add help message to a function, simply add a string after the function parameter in the signature.
```lisp
(fn add (x y)
  "Adds two numbers"
   (+ x y))
```
