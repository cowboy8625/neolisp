; (var value false)
; (fn main () (if value (print "then\n") (print "else\n")))

; (fn main () (print (- 321 123) "\n"))

; (fn apply (f x) (f x))
; (fn main () (print (apply (lambda (x) (+ x 321)) 123) "\n"))

; (var mul (lambda (x y) (* x y)))
(fn main () (print (/ 123 321) "\n"))

; (var mul (lambda (x) (lambda (y) (* x y))))
; (fn main () (print ((mul 123) 321) "\n"))

; (test "test if 123 is equal to 321" (assert-eq (= 123 321) false))
; (fn main () (print ((lambda (x y) (+ x y)) 123 321) "\n"))

; (fn add (x y) (+ x y))
; (fn main () (print (add 123 321) "\n"))
