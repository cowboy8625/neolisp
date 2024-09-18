; (var add (lambda (x y) (+ x y)))
; (fn main () (print (add 123 321) "\n"))

; (var add (lambda (x) (lambda (y) (+ x y))))
; (fn main () (print ((add 123) 321) "\n"))

; (fn add (x y) (+ x y))
(test "test if 123 is equal to 321" (assert-eq (= 123 321) false))
(fn main () (print ((lambda (x y) (+ x y)) 123 321) "\n"))
