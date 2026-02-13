; Language level implementations of core functions
; they are slower but fun to write
(fn nav-filter (f lst)
  (if (= 0 (length lst))
    '()
    (if (f (car lst))
      (cons (car lst) (nav-filter f (cdr lst)))
      (nav-filter f (cdr lst)))))

(fn nav-map (f lst)
  (if (= 0 (length lst))
    '()
    (cons (f (car lst))
      (nav-map f (cdr lst)))))

(fn nav-fold (acc f lst)
  (if (= 0 (length lst))
    acc
    (fold (f acc (car lst)) f (cdr lst))))

(fn nav-fold-right (acc f lst)
  (if (= 0 (length lst))
    acc
    (fold-right (f (car lst) acc) f (cdr lst))))

(fn some (item) '(:some item))
(fn some? (item) (= :some (car item)))
(fn none () '(:none))
(fn none? (item) (= :none (car item)))
(fn ok (item) '(:ok item))
(fn ok? (item) (= :ok (car item)))
(fn err (item) '(:err))
(fn err? (item) (= :err (car item)))
