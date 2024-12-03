(var a 123)
(var b 321)

(fn sub (x y) (- x y))

(var add (lambda (x y) (+ x y)))


(var c (add b a))
(var d (sub b a))

(print c ", " d "\n")

(if (> d 0)
  (print "d is positive\n")
  (print "d is negative\n"))

(var e '(1 2 3))
(print e "\n")

;; Builtin testing run with `neolisp test`
(test add (assert-eq :expected 3 :actual (add 1 2) :description "1 + 2 = 3"))
;; >> test add passed

(print 'this_is_a_quoted_var_name "\n")
;; >> this_is_a_quoted_var_name

