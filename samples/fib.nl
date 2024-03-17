;; Fibonacci sequence
;; this function takes 46 seconds to run on a Applle M1 Max
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(print (fib 30) "\n")
