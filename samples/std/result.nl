(fn ok (item) '(:ok item))
(fn ok? (item) (= :ok (car item)))
(fn err (item) '(:err))
(fn err? (item) (= :err (car item)))
