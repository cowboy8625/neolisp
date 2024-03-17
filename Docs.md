# NeoLisp Docs

## Functions

### **+**:
- returns a sum of the arguments
- arguments type: `Number`
- example:
```lisp
(+ 10 20) ; -> 30
```

### **-**:
- returns the difference of the arguments
- arguments type: `Number`
- example:
```lisp
(- 20 10) ; -> 10
```

### **=**:
- returns true if all the arguments are equal to the first
- arguments type: `Number`
- example:
```lisp
(= 10 10) ; -> true
```

### **>**:
- returns true if the first argument is greater than the rest
- arguments type: `Number`
- example:
```lisp
(> 10 10) ; -> false
```

### **<**:
- returns true if the first argument is less than the rest
- arguments type: `Number`
- example:
```lisp
(< 10 10) ; -> false
```

### **>=**:
- returns true if the first argument is greater than or equal to the rest
- arguments type: `Number`
- example:
```lisp
(>= 10 10) ; -> true
```

### **<=**:
- returns true if the first argument is less than or equal to the rest
- arguments type: `Number`
- example:
```lisp
(<= 10 10)
```

### **and**:
- returns true if all the arguments are true
- arguments type: `Bool`
- example:
```lisp
(print 10)
```
### **or**:
- returns true if any of the arguments are true
- arguments type: `Bool`
- example:
```lisp
(print 10)
```

### **not**:
- returns the opposite of the first argument
- arguments type: `Bool`
- example:
```lisp
(print 10)
```

### **print**:
- returns the first argument
- arguments type: `Any`
- example:
```lisp
(print 10)
```

### **typeof**:
- returns the type of the first argument
- argument type: `Any`
- example:
```lisp
(typeof 10) ; -> Number
```

### **help**:
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

### **list**:
- returns a `List` of elements
- arguments type: `Any`
- example:
```lisp
(list 1 2 3) ; -> (1 2 3)
```

### **cons**:
- returns a list with an element added to the beginning
- arguments
   1. item to add
   2. List
- example:
```lisp
(cons 1 (list 2 3)) ; -> (1 2 3)
```

### **car**:
- returns the first element of a list
- argument
   1. List
- example:
```lisp
(car (list 1 2 3)) ; -> 1
```

### **cdr**:
- returns a list without the first element
- argument
   1. List
- example:
```lisp
(cdr (list 1 2 3)) ; -> (2 3)
```

### **append**:
- returns a `List` with elements appended
- arguments type: `List`
- example:
```lisp
(append (list 1 2) (list 3 4)) ; -> (1 2 3 4)
```

### **reverse**:
- returns a list with elements in reverse order
- argument type: `List`
- example:
```lisp
(reverse (list 1 2 3)) ; -> (3 2 1)
```

### **nth**:
- returns the nth element of a list
- arguments type: `List`
- example:
```lisp
(nth (list 1 2 3) 2) ; -> 3
```

### **length**:
- returns the length of a list
- argument type: `List`
- example:
```lisp
(length (list 1 2 3)) ; -> 3
```

### **map**:
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

## Adding Help message to function

To add help message to a function, simply add a string after the function parameter in the signature.
```lisp
(fn add (x y)
  "Adds two numbers"
   (+ x y))
```
