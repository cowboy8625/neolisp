;; Fibonacci sequence
;; this function takes 8.66 seconds to run on a Applle M1 Max
;; with the new vm this takes 0.5 seconds to run

;; 21 58 95 169 280 465 761 1241 2019 3277 5312 8605
;; (fn fib1 (n)
;;   (if (or (= n 0) (= n 1))
;;       n
;;       (+ (fib1 (- n 1)) (fib1 (- n 2)))))
;; (fn main () (print (fib1 30) "\n"))

;; ---------------------------------------

;; This is much faster with tail call optimization 0.003
(fn fib2 (n a b)
  (if (= n 0)
      a
      (fib2 (- n 1) b (+ a b))))
(fn main () (print (fib2 30 0 1) "\n"))

;; ----------------------------------------

;; (fn fib3 (n)
;;   (if (= n 0)
;;       0
;;       (if (= n 1)
;;           1
;;           (+ (fib3 (- n 1)) (fib3 (- n 2))))))
;; (fn main () (print (fib3 30) "\n"))

;; ----------------------------------------

;; (fn main () (print (fib1 30) ", " (fib2 30 0 1) ", " (fib3 30) "\n"))

