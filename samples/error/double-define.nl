; This should throw an error
(fn add (a b) (+ a b))
(fn add (x y) (+ x y))
(fn main () (print "define the same function name twice should error\n"))

(test add (assert-eq :expected 3 :actual (add 1 2) :description "add 1 and 2"))
(test add (assert-eq :expected 3 :actual (add 1 2) :description "add 1 and 2"))
