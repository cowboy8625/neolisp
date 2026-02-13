(fn closure-test ()
  (let ((x 42))
    ((lambda () x))))

(test closure-test
  (assert-eq
    :expected 42
    :actual (closure-test)
    :description "closure capture failed"))

(fn make-adder (z)
  (lambda (y) (+ z y)))

(test make-adder
  (assert-eq
    :expected 7
    :actual ((make-adder 3) 4)
    :description "returned closure failed"))

(fn main () ((make-adder 3) 4))
