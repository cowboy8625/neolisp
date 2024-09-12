(fn add (x y) (+ x y ))
(test "This is a test of function add"
  (assert-eq (add 1 3) 444))
(fn main () (print (add 123 321)))
