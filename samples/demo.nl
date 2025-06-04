; This file servers as a demo of what neolisp can do
; while also having a test suite in the language.

(var a 123)
(var b 321)

(test b-a (assert-eq :expected 198 :actual (- b a) :description "321 - 123 = 198"))

; map is builtin but you can also make your own
(fn native-map (f lst)
  (if (= 0 (length lst))
    '()
    (cons (f (car lst))
      (native-map f (cdr lst)))))

(test native-map
  (assert-eq
    :expected '(2 3 4)
    :actual (native-map (lambda (x) (+ x 1)) '(1 2 3))
    :description "should add 1 to each list element"))

(var add (lambda (x y) (+ x y)))
;; Builtin testing run with `neolisp test`
(test add (assert-eq :expected 3 :actual (add 1 2) :description "1 + 2 = 3"))
;; >> test add passed

(fn range-helper (n acc)
  (if (<= n 0)
      acc
      (range-helper (- n 1) (cons (- n 1) acc))))

(fn range (n)
  (range-helper n '()))

(test range
  (assert-eq
    :expected (list 0 1 2 3)
    :actual (range 4)
    :description "range n: 4 return (0 1 2 3)"))
