(fn add (x y) (+ x y ))
(test "This is a test of function add"
  (assert-eq (add 123 321) 444))
(fn main () (print (add 123 321) "\n"))
