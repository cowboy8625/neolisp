(fn closure-test ()
  (let ((x 42))
    ((lambda () x))))

(test closure-test
  (assert-eq
    :expected 42
    :actual (closure-test)
    :description "closure capture failed"))
