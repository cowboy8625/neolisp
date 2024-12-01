; create a list of numbers from 0 to n
; recursively create a list of numbers from 0 to n
; this returns a reverse list
;; (fn range (n)
;;     (if (> n 0)
;;       (cons (- n 1) (range (- n 1)))
;;       (list)))


(fn range (n)
  (var range-helper (lambda (n acc)
    (if (> n 0)
        (range-helper (- n 1) (cons (- n 1) acc))
        acc)))
  (range-helper n (list)))

(test test-range
  (assert-eq :expected (list 3 2 1 0) :actual (range 4) :description "range n: 4 return (3 2 1 0)")
  ;; (assert-eq (list 0 1 2 3) (reverse (range 4)) "range n: 4 return (0 1 2 3)")
)
; (assert-eq (list 3 2 1 0) (range 4) "range n: 4 return (3 2 1 0)")
; (assert-eq (list 0 1 2 3) (reverse (range 4)) "range n: 4 return (0 1 2 3)")

; Current pattern           111 110 101 100 011 010 001 000
; New state for center cell  0   1   1   0   1   1   1   0 ; returns 0..7 for all the possibile states the three cells can be in
(fn cal-rule (a b c) (+ (* a 4) (* b 2) (* c 1)))

; (assert-eq 0 (cal-rule 0 0 0) "cal-rule 0 0 0") ; 0
; (assert-eq 1 (cal-rule 0 0 1) "cal-rule 0 0 1") ; 1
; (assert-eq 2 (cal-rule 0 1 0) "cal-rule 0 1 0") ; 1
; (assert-eq 3 (cal-rule 0 1 1) "cal-rule 0 1 1") ; 1
; (assert-eq 4 (cal-rule 1 0 0) "cal-rule 1 0 0") ; 0
; (assert-eq 5 (cal-rule 1 0 1) "cal-rule 1 0 1") ; 1
; (assert-eq 6 (cal-rule 1 1 0) "cal-rule 1 1 0") ; 1
; (assert-eq 7 (cal-rule 1 1 1) "cal-rule 1 1 1") ; 0
; ----------------------------------------------------


(fn is-alive (id)
    (if (= id 0) 0
    (if (= id 1) 1
    (if (= id 2) 1
    (if (= id 3) 1
    (if (= id 4) 0
    (if (= id 5) 1
    (if (= id 6) 1
    (if (= id 7) 0
      ; else
      0)))))))))

; (assert-eq 0 (is-alive 0) "is-alive 0 -> 0")
; (assert-eq 1 (is-alive 1) "is-alive 1 -> 1")
; (assert-eq 1 (is-alive 2) "is-alive 2 -> 1")
; (assert-eq 1 (is-alive 3) "is-alive 3 -> 1")
; (assert-eq 0 (is-alive 4) "is-alive 4 -> 0")
; (assert-eq 1 (is-alive 5) "is-alive 5 -> 1")
; (assert-eq 1 (is-alive 6) "is-alive 6 -> 1")
; (assert-eq 0 (is-alive 7) "is-alive 7 -> 0")
; ----------------------------------------------------


; Returns the index of the cell in the grid and will wrap around either end
(fn get-cell (grid i)
  (mod (+ i (length grid)) (length grid)))

; (assert-eq 2 (get-cell (list 'a 'b 'c) -1) "get-cell list len 3, index -1, returns 2")
; (assert-eq 0 (get-cell (list 'a 'b 'c)  0) "get-cell list len 3, index  0, returns 0")
; (assert-eq 1 (get-cell (list 'a 'b 'c)  1) "get-cell list len 3, index  1, returns 1")
; (assert-eq 2 (get-cell (list 'a 'b 'c)  2) "get-cell list len 3, index  2, returns 2")
; (assert-eq 0 (get-cell (list 'a 'b 'c)  3) "get-cell list len 3, index  3, returns 0")
; ----------------------------------------------------

; Returns the state of the cells next generation
(fn generation-of-cell (grid index)
  (let
    ((a (nth grid (get-cell grid (- index 1))))
    (b (nth grid (get-cell grid index)))
    (c (nth grid (get-cell grid (+ index 1)))))
    (is-alive (cal-rule a b c))))

(fn main () (print (generation-of-cell (list 0 0 0 1) 0) "\n"))

; (assert-eq
;   0 ; expected
;   ;                         ↓
;   (generation-of-cell (list 0 0 0 1) 0) ; actual
;   "generation-of-cell, grid: 0 0 0 1, index 0, returns 0") ; description
; (assert-eq
;   0 ; expected
;   ;                           ↓
;   (generation-of-cell (list 0 0 0 1) 1) ; actual
;   "generation-of-cell, grid: 0 0 0 1, index 1, returns 0") ; description
; (assert-eq
;   1 ; expected
;   ;                             ↓
;   (generation-of-cell (list 0 0 0 1) 2) ; actual
;   "generation-of-cell, grid: 0 0 0 1, index 2, returns 1") ; description
; (assert-eq
;   1 ; expected
;   ;                               ↓
;   (generation-of-cell (list 0 0 0 1) 3) ; actual
;   "generation-of-cell, grid: 0 0 0 1, index 3, returns 1") ; description
; ----------------------------------------------------

; Returns the next generation of the grid
;;;; (fn next-generation (grid)
;;;;   (map
;;;;     (lambda (i) (generation-of-cell grid i))
;;;;     (reverse (range (length grid)))
;;;; ))

; (assert-eq
;   (list 0 0 1 1) ; expected
;   (next-generation (list 0 0 0 1)) ; actual
;   "next-generation, grid: 0 0 0 1, returns 0 0 1 1") ; description


; ----------------------------------------------------

; Loop
;;;; (fn main ()
;;;;   (let
;;;;     ((grid (append (map (lambda (x) 0) (range 100)) (list 1)))
;;;;     (is-running true))
;;;;     (loop is-running
;;;;       (var grid (next-generation grid))
;;;;       ; (print grid)
;;;;       (let
;;;;         ((line (to-string (map (lambda (x) (if (= x 1) "*" " ")) grid))))
;;;;           (print line "\n")))))

; Testing the creation of the grid
; (assert-eq
;   "    *" ; expected
;   (let
;     ((grid (append (map (lambda (x) 0) (range 4)) '(1))))
;     (to-string (map (lambda (x) (if (= x 1) "*" " ")) grid)) ; actual
;   )
;   "to-string, grid: 0 0 0 0 1, returns 00001") ; description
