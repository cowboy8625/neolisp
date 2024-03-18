; Current pattern           111 110 101 100 011 010 001 000
; New state for center cell  0   1   1   0   1   1   1   0
(fn cal-rule (a b c) (
	(+ (* a 4) (* b 2) (* c 1))))

(assert (= 0 (cal-rule 0 0 0)))
(assert (= 1 (cal-rule 0 0 1)))
(assert (= 1 (cal-rule 0 1 0)))
(assert (= 0 (cal-rule 0 1 1)))
(assert (= 1 (cal-rule 1 0 0)))
(assert (= 1 (cal-rule 1 0 1)))
(assert (= 1 (cal-rule 1 1 0)))
(assert (= 1 (cal-rule 1 1 1)))
