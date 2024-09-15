(var add (lambda (x y) (+ x y)))
(test "test if 123 is equal to 321" (assert-eq (= 123 321) false))
(fn main () (print (add 123 321)))
