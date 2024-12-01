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

(test test-add (assert (= (add 1 2) 4) "test-add"))
;; >> 1 2 3

;; (print 'this_is_a_quoted_var_name "\n")
;; >> this_is_a_quoted_var_name
