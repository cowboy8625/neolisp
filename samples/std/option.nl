(fn some (item) (list :some item))
(fn some? (item) (= :some (car item)))
(fn none () '(:none))
(fn none? (item) (= :none (car item)))

(fn unwrap (item) (last item))
(test unwrap-some
  (assert-eq
    :expected 1
    :actual (unwrap (some 1))
    :description "unwrap (some 1) -> 1"))
(test unwrap-none
  (assert-eq
    :expected :none
    :actual (unwrap (none))
    :description "unwrap (none) -> :none"))


(fn unwrap-or (item default) (if (some? item) (last item) default))
(test unwrap-or-some
  (assert-eq
    :expected 1
    :actual (unwrap-or (some 1) 0)
    :description "unwrap-or (some 1) 0 -> 1"))
(test unwrap-or-none
  (assert-eq
    :expected 0
    :actual (unwrap-or (none) 0)
    :description "unwrap-or (none) 0 -> 0"))


(fn option-map (item f) (if (none? item) (none) (some (f (unwrap item)))))
(test option-map-some
  (assert-eq
    :expected '(:some 2)
    :actual (option-map (some 1) (lambda (x) (+ x 1)))
    :description "option-map (some 1) (lambda (x) (+ x 1)) -> (:some 1)"))
(test option-map-none
  (assert-eq
    :expected '(:none)
    :actual (option-map (none) (lambda (x) (+ x 1)))
    :description "option-map (none) (lambda (x) (+ x 1)) -> (:none)"))
