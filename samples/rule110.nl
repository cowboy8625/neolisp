; TODO: make so that if a assert fails then the test fails
; currently the only assert that matters is the last one.

; Helpers for testing
; test is a list of assert-eq or assert
(fn it (tests) (fold false (lambda (acc value) (or acc value)) tests))

; ----------------------------------------------------

; create a list of numbers from 0 to n
; recursively create a list of numbers from 0 to n
; this returns a reverse list
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

; ----------------------------------------------------

; Current pattern           111 110 101 100 011 010 001 000
; New state for center cell  0   1   1   0   1   1   1   0
; returns 0..7 for all the possibile states the three cells can be in
(fn cal-rule (a b c) (+ (* a 4) (* b 2) c))

(test cal-rule
  (it
    (list
      (assert-eq :expected 0 :actual (cal-rule 0 0 0) :description "cal-rule 0 0 0")    ; 0
      (assert-eq :expected 1 :actual (cal-rule 0 0 1) :description "cal-rule 0 0 1")    ; 1
      (assert-eq :expected 2 :actual (cal-rule 0 1 0) :description "cal-rule 0 1 0")    ; 1
      (assert-eq :expected 3 :actual (cal-rule 0 1 1) :description "cal-rule 0 1 1")    ; 1
      (assert-eq :expected 4 :actual (cal-rule 1 0 0) :description "cal-rule 1 0 0")    ; 0
      (assert-eq :expected 5 :actual (cal-rule 1 0 1) :description "cal-rule 1 0 1")    ; 1
      (assert-eq :expected 6 :actual (cal-rule 1 1 0) :description "cal-rule 1 1 0")    ; 1
      (assert-eq :expected 7 :actual (cal-rule 1 1 1) :description "cal-rule 1 1 1") ))); 0

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

(test is-alive
  (it
    (list
      (assert-eq :expected 0 :actual (is-alive 0) :description "is-alive 0 -> 0")
      (assert-eq :expected 1 :actual (is-alive 1) :description "is-alive 1 -> 1")
      (assert-eq :expected 1 :actual (is-alive 2) :description "is-alive 2 -> 1")
      (assert-eq :expected 1 :actual (is-alive 3) :description "is-alive 3 -> 1")
      (assert-eq :expected 0 :actual (is-alive 4) :description "is-alive 4 -> 0")
      (assert-eq :expected 1 :actual (is-alive 5) :description "is-alive 5 -> 1")
      (assert-eq :expected 1 :actual (is-alive 6) :description "is-alive 6 -> 1")
      (assert-eq :expected 0 :actual (is-alive 7) :description "is-alive 7 -> 0"))))

; ----------------------------------------------------

; Returns the index of the cell in the grid and will wrap around either end
(fn get-cell (grid i)
  (mod (+ i (length grid)) (length grid)))

(test get-cell
  (it
    (list
      (assert-eq :expected 2 :actual (get-cell (list 'a 'b 'c) -1) :description "get-cell list len 3, index -1, returns 2")
      (assert-eq :expected 0 :actual (get-cell (list 'a 'b 'c)  0) :description "get-cell list len 3, index  0, returns 0")
      (assert-eq :expected 1 :actual (get-cell (list 'a 'b 'c)  1) :description "get-cell list len 3, index  1, returns 1")
      (assert-eq :expected 2 :actual (get-cell (list 'a 'b 'c)  2) :description "get-cell list len 3, index  2, returns 2")
      (assert-eq :expected 0 :actual (get-cell (list 'a 'b 'c)  3) :description "get-cell list len 3, index  3, returns 0")
      (assert-eq :expected 1 :actual (get-cell (list 'a 'b 'c)  4) :description "get-cell list len 3, index  4, returns 1")
      (assert-eq :expected 2 :actual (get-cell (list 'a 'b 'c)  5) :description "get-cell list len 3, index  5, returns 2")
      (assert-eq :expected 0 :actual (get-cell (list 'a 'b 'c)  6) :description "get-cell list len 3, index  6, returns 0")
      (assert-eq :expected 1 :actual (get-cell (list 'a 'b 'c)  7) :description "get-cell list len 3, index  7, returns 1"))))

; ----------------------------------------------------

; Returns the state of the cells next generation
; (fn generation-of-cell (grid index)
;   (is-alive
;   (cal-rule
;     (nth grid (get-cell grid (- index 1)))
;     (nth grid (get-cell grid index))
;     (nth grid (get-cell grid (+ index 1))))))

(fn generation-of-cell (grid index)
  (let
    ((a (nth grid (get-cell grid (- index 1))))
      (b (nth grid (get-cell grid index)))
      (c (nth grid (get-cell grid (+ index 1)))))
    (is-alive (cal-rule a b c))))


(test generation-of-cell
  (it
    (list
      (assert-eq
        :expected 0
        :actual (generation-of-cell (list 0 0 0 1) 0)
        :description  "generation-of-cell, grid: 0 0 0 1, index 0, returns 0")
      (assert-eq
        :expected 0
        :actual (generation-of-cell (list 0 0 0 1) 1)
        :description "generation-of-cell, grid: 0 0 0 1, index 1, returns 0")
      (assert-eq
        :expected 1
        :actual (generation-of-cell (list 0 0 0 1) 2)
        :description "generation-of-cell, grid: 0 0 0 1, index 2, returns 1")
      (assert-eq
        :expected 1
        :actual (generation-of-cell (list 0 0 0 1) 3)
        :description "generation-of-cell, grid: 0 0 0 1, index 3, returns 1"))))

; ----------------------------------------------------

; Returns the next generation of the grid
(fn next-generation (grid)
  (map
    (lambda (i) (generation-of-cell grid i))
    (range (length grid))))


(test next-generation
  (assert-eq
    :expected (list 0 0 1 1)
    :actual (next-generation (list 0 0 0 1))
    :description "next-generation, grid: 0 0 0 1, returns 0 0 1 1"))

; ----------------------------------------------------

(fn main ()
  (let
    ((grid (append (map (lambda (x) 0) (range 5)) (list 1)))
    (is-running true))
    (loop is-running
      (set grid (next-generation grid))
      (let
        ((line (to-string (map (lambda (x) (if (= x 1) "*" " ")) grid))))
          (print line "\n")))))
