(var a 123)
(var b 321)

(fn sub (x y) (- x y))

(var add (lambda (x y) (+ x y)))

(var c (add b a))
(var b (sub b a))

(print c ", " b "\n")

(if (> b 0)
	(print "b is positive\n")
	(print "b is negative\n"))

(var d '(1 2 3))
(print d "\n")
;; >> 1 2 3

(print 'this_is_a_quoted_var_name "\n")
;; >> this_is_a_quoted_var_name
