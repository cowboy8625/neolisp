; (test sleep ; not sure how to test this

; operator functions

(test add+2
  (assert-eq
    :expected 3
    :actual (+ 1 2)
    :description "+ 1 2 -> 3"))

(test add+50
  (assert-eq
    :expected 1275
    :actual (+ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description "+ 1..50 -> 1275"))

(test sub-2
  (assert-eq
    :expected -1
    :actual (- 1 2)
    :description "- 1 2 -> -1"))

(test sub-50
  (assert-eq
    :expected -1273
    :actual (- 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description "- 1..50 -> -1273"))

(test mul*2
  (assert-eq
    :expected 2
    :actual (* 1 2)
    :description "* 1 2 -> 2"))

(test mul*50
  (assert-eq
    :expected
    30414093201713376000000000000000000000000000000000000000000000000
    :actual (* 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description
    "* 1..50 -> 30414093201713376000000000000000000000000000000000000000000000000"))

(test div/2
  (assert-eq
    :expected 0.5
    :actual (/ 1 2)
    :description "/ 1 2 -> 0.5"))

(test div/50
  (assert-eq
    :expected
    0.000000000000000000000000000000000000000000000000000000000000000032879494166331576
    :actual (/ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description
    "* 1..50 -> 0.000000000000000000000000000000000000000000000000000000000000000032879494166331576"))

(test equal=2-false
  (assert-eq
    :expected false
    :actual (= 1 2)
    :description "= 1 2 -> false"))

(test equal=2-true
  (assert-eq
    :expected true
    :actual (= 1 1)
    :description "= 1 1 -> true"))

(test equal=2-nil
  (assert-eq
    :expected nil
    :actual (= 1 'a)
    :description "= 1 'a -> nil"))

(test equal=50
  (assert-eq
    :expected false
    :actual (= 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description "* 1..50 -> false"))

(test greater>2-false
  (assert-eq
    :expected false
    :actual (> 1 2)
    :description "> 1 2 -> false"))

(test greater>2-true
  (assert-eq
    :expected true
    :actual (> 2 1)
    :description "> 2 1 -> true"))

(test greater>2-nil
  (assert-eq
    :expected nil
    :actual (> 1 'a)
    :description "> 1 'a -> nil"))

(test greater>50
  (assert-eq
    :expected false
    :actual (> 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description "> 1..50 -> false"))

(test less<2-true
  (assert-eq
    :expected true
    :actual (< 1 2)
    :description "< 1 2 -> true"))

(test less<2-false
  (assert-eq
    :expected false
    :actual (< 2 1)
    :description "> 2 1 -> false"))

(test less<2-nil
  (assert-eq
    :expected nil
    :actual (< 1 'a)
    :description "< 1 'a -> nil"))

(test less<50
  (assert-eq
    :expected true
    :actual (< 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description "> 1..50 -> false"))

(test greater-or-equal>=2-false
  (assert-eq
    :expected false
    :actual (>= 1 2)
    :description "=> 1 2 -> false"))

(test greater-or-equal>=2-true
  (assert-eq
    :expected true
    :actual (>= 2 1)
    :description ">= 2 1 -> true"))

(test greater-or-equal>=2-nil
  (assert-eq
    :expected nil
    :actual (>= 1 'a)
    :description ">= 1 'a -> nil"))

(test greater-or-equal=>50
  (assert-eq
    :expected false
    :actual (>= 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
               21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37
               38 39 40 41 42 43 44 45 46 47 48 49 50)
    :description ">= 1..50 -> false"))

; TODO: not will fail at runtime if a bool is not given

(test not-false-true
  (assert-eq
    :expected true
    :actual (not false)
    :description "not false -> true"))

(test not-true-false
  (assert-eq
    :expected false
    :actual (not true)
    :description "not true -> false"))

; TODO: Again and will fail at runtime if a bool is not given

(test and-2-true
  (assert-eq
    :expected true
    :actual (and true true)
    :description "and true true -> true"))

(test and-2-false
  (assert-eq
    :expected false
    :actual (and true false)
    :description "and true false -> false"))

(test and-5-true
  (assert-eq
    :expected true
    :actual (and true true true true true)
    :description "and true true true true true -> true"))

(test and-5-false
  (assert-eq
    :expected false
    :actual (and true true false true true)
    :description "and true true false true true -> false"))

; TODO: Again or will fail at runtime if a bool is not given

(test or-2-true
  (assert-eq
    :expected true
    :actual (or true false)
    :description "or true false -> true"))

(test or-2-false
  (assert-eq
    :expected false
    :actual (or false false)
    :description "and false false -> false"))

(test or-5-true
  (assert-eq
    :expected true
    :actual (or false false false false true)
    :description "or false false false false true -> true"))

(test or-5-false
  (assert-eq
    :expected false
    :actual (or false false false false false)
    :description "or false false false false false -> false"))

; ---------- Builtin functions ----------

; This function gets used in the test below
(fn add (x y) (+ x y))

(test atom?-symbol
  (assert-eq
    :expected true
    :actual (atom? 'a)
    :description "atom? 'a -> true"))

(test atom?-key
  (assert-eq
    :expected true
    :actual (atom? :key)
    :description "atom? :key -> true"))

(test atom?-f32
  (assert-eq
    :expected true
    :actual (atom? 123.321)
    :description "atom? 123.321 -> true"))

(test atom?-bool-true
  (assert-eq
    :expected true
    :actual (atom? true)
    :description "atom? true -> true"))

(test atom?-bool-false
  (assert-eq
    :expected true
    :actual (atom? false)
    :description "atom? false -> true"))

(test atom?-string
  (assert-eq
    :expected true
    :actual (atom? "Hello, world!")
    :description "atom? \"Hello, world!\" -> true"))

(test atom?-q-list
  (assert-eq
    :expected false
    :actual (atom? '(1 2 3))
    :description "atom? '(1 2 3) -> false"))

(test atom?-list
  (assert-eq
    :expected false
    :actual (atom? (list 1 2 3))
    :description "atom? (list 1 2 3) -> false"))

(test atom?-lambda
  (assert-eq
    :expected false
    :actual (atom? (lambda (x y) (+ x y)))
    :description "atom? (lambda (x y) (+ x y)) -> false"))

(test atom?-function
  (assert-eq
    :expected false
    :actual (atom? add)
    :description "atom? add -> false ; is a function"))

(test number?-f32
  (assert-eq
    :expected true
    :actual (number? 123.321)
    :description "number? 123.321 -> true"))

(test number?-symbol
  (assert-eq
    :expected false
    :actual (number? 'a)
    :description "number? 'a -> false"))

(test number?-key
  (assert-eq
    :expected false
    :actual (number? :key)
    :description "number? :key -> false"))

(test number?-bool-true
  (assert-eq
    :expected false
    :actual (number? true)
    :description "number? true -> false"))

(test number?-bool-false
  (assert-eq
    :expected false
    :actual (number? false)
    :description "number? false -> false"))

(test number?-string
  (assert-eq
    :expected false
    :actual (number? "Hello, world!")
    :description "number? \"Hello, world!\" -> false"))

(test number?-q-list
  (assert-eq
    :expected false
    :actual (number? '(1 2 3))
    :description "number? '(1 2 3) -> false"))

(test number?-list
  (assert-eq
    :expected false
    :actual (number? (list 1 2 3))
    :description "number? (list 1 2 3) -> false"))

(test number?-lambda
  (assert-eq
    :expected false
    :actual (number? (lambda (x y) (+ x y)))
    :description "number? (lambda (x y) (+ x y)) -> false"))

(test number?-function
  (assert-eq
    :expected false
    :actual (number? add)
    :description "number? add -> false ; is a function"))

(test slice-list
  (assert-eq
    :expected '(2 3)
    :actual (slice '(1 2 3 4 5) 1 3)
    :description "slice '(1 2 3 4 5) 1 3 -> (2 3)"))

(test slice-string
  (assert-eq
    :expected "hello"
    :actual (slice "hello world" 0 5)
    :description "slice \"hello world\" 0 5 -> \"hello\""))

(test split-string
  (assert-eq
    :expected '("1" "2" "3")
    :actual (split " " "1 2 3" )
    :description "split \"1 2 3\" \" \" -> (\"1\" \"2\" \"3\")"))

(test join
  (assert-eq
    :expected "1,2,3"
    :actual (join "," '(1 2 3))
    :description "join \" \" '(1 2 3) -> \"1,2,3\""))

(test to-string-number
  (assert-eq
    :expected "123"
    :actual (to-string 123)
    :description "to-string 123 -> \"123\""))

(test to-string-symbol
  (assert-eq
    :expected "this-is-a-symbol"
    :actual (to-string 'this-is-a-symbol)
    :description "to-string 'this-is-a-symbol -> \"this-is-a-symbol\""))

(test filter
  (assert-eq
    :expected '(2 4 6 8)
    :actual (filter (lambda (x) (= (mod x 2) 0)) '(1 2 3 4 5 6 7 8 9))
    :description "filter even? '(1 2 3 4 5 6 7 8 9) -> (2 4 6)"))

(test fold-right
  (assert-eq
    :expected 15
    :actual (fold-right 0 (lambda (x y) (+ x y)) '(1 2 3 4 5))
    :description "fold-right 0 (lambda (x y) (+ x y)) '(1 2 3 4 5) -> 15"))

(test fold
  (assert-eq
    :expected 15
    :actual (fold 0 (lambda (x y) (+ x y)) '(1 2 3 4 5))
    :description "fold 0 (lambda (x y) (+ x y)) '(1 2 3 4 5) -> 15"))

(test map
  (assert-eq
    :expected '(2 4 6 8)
    :actual (map (lambda (x) (* x 2)) '(1 2 3 4))
    :description "map (lambda (x) (* x 2)) '(1 2 3 4) -> (2 4 6 8)"))

(test nth-list
  (assert-eq
    :expected 2
    :actual (nth '(1 2 3 4) 1)
    :description "nth '(1 2 3 4) 1 -> 2"))

(test nth-string
  (assert-eq
    :expected "2"
    :actual (nth "1234" 1)
    :description "nth \"1234\" 1 -> 2"))

(test reverse-list
  (assert-eq
    :expected '(4 3 2 1)
    :actual (reverse '(1 2 3 4))
    :description "reverse '(1 2 3 4) -> (4 3 2 1)"))

(test reverse-string
  (assert-eq
    :expected "4321"
    :actual (reverse "1234")
    :description "reverse \"1234\" -> \"4321\""))

(test append-list
  (assert-eq
    :expected '(1 2 3 4 5)
    :actual (append '(1 2) '(3 4 5))
    :description "append '(1 2) '(3 4 5) -> (1 2 3 4 5)"))

(test append-string
  (assert-eq
    :expected "hello world"
    :actual (append "hello" " world")
    :description "append \"hello\" \" world\" -> \"hello world\""))

(test last-list
  (assert-eq
    :expected 5
    :actual (last '(1 2 3 4 5))
    :description "last '(1 2 3 4 5) -> 5"))

(test last-string
  (assert-eq
    :expected "5"
    :actual (last "12345")
    :description "last \"12345\" -> 5"))

(test cdr-list
  (assert-eq
    :expected '(2 3 4 5)
    :actual (cdr '(1 2 3 4 5))
    :description "cdr '(1 2 3 4 5) -> (2 3 4 5)"))

(test type?-f64
  (assert-eq
    :expected "f64"
    :actual (type? 123)
    :description "type? 123 -> \"f64\""))

(test type?-symbol
  (assert-eq
    :expected "Symbol"
    :actual (type? 'a)
    :description "type? 'a -> \"symbol\""))

(test type?-:keyword
  (assert-eq
    :expected "Keyword"
    :actual (type? :key)
    :description "type? :key -> \"Keyword\""))

(test type?-bool-true
  (assert-eq
    :expected "Bool"
    :actual (type? true)
    :description "type? true -> \"Bool\""))

(test type?-bool-false
  (assert-eq
    :expected "Bool"
    :actual (type? false)
    :description "type? false -> \"Bool\""))

(test type?-string
  (assert-eq
    :expected "String"
    :actual (type? "hello")
    :description "type? \"hello\" -> \"String\""))

(test type?-list
  (assert-eq
    :expected "List"
    :actual (type? '(1 2 3))
    :description "type? '(1 2 3) -> \"List\""))

(test type?-lambda
  (assert-eq
    :expected "Function"
    :actual (type? (lambda (x) (+ x 1)))
    :description "type? (lambda (x) (+ x 1)) -> \"Function\""))

(test type?-function
  (assert-eq
    :expected "Function"
    :actual (type? add)
    :description "type? add -> \"Function\""))

; TODO: at some point we need to be able to capture stdout
; (test print
;   (assert-eq
;     :expected "Hello, world!\n"
;     :actual (print "Hello, world!\n")
;     :description "print \"Hello, world!\n\" -> \"Hello, world!\n\""))

(test length-list
  (assert-eq
    :expected 5
    :actual (length '(1 2 3 4 5))
    :description "length '(1 2 3 4 5) -> 5"))

(test length-string
  (assert-eq
    :expected 5
    :actual (length "hello")
    :description "length \"hello\" -> 5"))

(test assert (assert true "assert true -> true"))

(test list
  (assert-eq
    :expected '(1 2 3)
    :actual (list 1 2 3)
    :description "list 1 2 3 -> (1 2 3)"))

(test cons
  (assert-eq
    :expected '(1 2)
    :actual (cons 1 '(2))
    :description "cons 1 2 -> (1 . 2)"))

(test car
  (assert-eq
    :expected 1
    :actual (car '(1 2 3))
    :description "car '(1 2 3) -> 1"))


; let binding

; (fn get-cell (grid i)
;   (let
;     ((l (length grid))
;     (wrapped (mod (+ i l) l)))
;       ; print will leave a string on the stack but once the function frame is destroyed
;       ; the string will get removed with the stack as its life time is tyed to it
;       (print "get-cell: " i " -> " wrapped "\n")
;       ; return the value we care about
;       wrapped))
;
; (test let-binding (assert-eq :expected 2 :actual (get-cell (list 'a 'b 'c) -1) :description "get-cell list len 3, index -1, returns 2"))

(fn main ()
  (print "This is a test file\n")
  (print "\x1b[32mRun with `neolisp test test.nl`\x1b[0m\n"))
