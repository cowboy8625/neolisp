;; Fibonacci sequence
;; this function takes 8.66 seconds to run on a Applle M1 Max
;; with the new vm this takes 0.3 seconds to run
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn main () (print (fib 30) "\n"))
;; 21 58 95 169 280 465 761 1241 2019 3277 5312 8605
; (fn tail_recursive_factorial (n acc)
;   (if (= n 0)
;       acc
;       (tail_recursive_factorial (- n 1) (* n acc))))
; 
; (fn main () (print (tail_recursive_factorial 30) "\n"))
