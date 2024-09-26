;; Fibonacci sequence
;; this function takes 8.66 seconds to run on a Applle M1 Max
;; with the new vm this takes 0.3 seconds to run
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn main () (print (fib 30) "\n"))
