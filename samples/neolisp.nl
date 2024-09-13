(fn parse-number (input)
  (let ((number '()))
    (loop (and (not (= 0 (length input))) (number? (car input)))
      (var number
           (append
             number
             (list (car input))))
      (var input (cdr input))
      (list (to-string number) input))))

(assert-eq (parse-number "123") '("123" "") "parse-number failed")
(assert-eq (parse-number "123abc") '("123" "abc") "parse-number failed")
; -------------------------------------------------------------------------

(fn letter? (expected)
    (let
      ((letters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
      (truth-table (map (lambda (letter) (= letter expected)) letters)))
        (fold false or truth-table)))

(assert-eq (letter? "1") false "test letter? failed 1")
(assert-eq (letter? "(") false "test letter? failed 2")
(assert-eq (letter? "z") true "test letter? failed 3")
(assert-eq (letter? ">") false "test letter? failed 4")
; -------------------------------------------------------------------------

(fn punctuation? (expected)
    "char -> bool"
    (let
      ((punctuations "!$,_-./:?+<=>#%&*@[\\]{|}`^~\"#;") ;"; this is just for my syntax highlighting to kick back in
      (truth-table (map (lambda (p) (= p expected)) punctuations)))
        (fold false or truth-table)))

(assert-eq (punctuation? "1") false "test punctuation? failed 1")
(assert-eq (punctuation? "(") false "test punctuation? failed 2")
(assert-eq (punctuation? "z") false "test punctuation? failed 3")
(assert-eq (punctuation? ">") true "test punctuation? failed 4")
(assert-eq (punctuation? "\"") true "test punctuation? failed 5")
; -------------------------------------------------------------------------

(fn is-identifier? (input) (or (letter? input) (punctuation? input) (number? input)))

(assert-eq (is-identifier? "-") true "test is-identifier? function failed '-'")
(assert-eq (is-identifier? "1") true "test is-identifier? function failed '1'")
(assert-eq (is-identifier? "?") true "test is-identifier? function failed '?'")
(assert-eq (is-identifier? "a") true "test is-identifier? function failed 'a'")
(assert-eq (is-identifier? " ") false "false is-identifier? function failed ' '")
; -------------------------------------------------------------------------

(fn parse-identifier (input)
  "accumulate the identifier in the input and return the parsed identifier and left over input"
  (let
    ((identifier '()))
    (do
      (loop (or (> 0 (length input)) (is-identifier? (car input)))
        (do
          (var identifier (append identifier (list (car input))))
          (var input (cdr input))
          (print "input='" input "'\n")))
      (list (to-string identifier) input))))

; Test cases
(assert-eq (parse-identifier "a123457bc") '("a123457bc" "") "parse-identifier failed")
(assert-eq (parse-identifier "abc") '("abc" "") "parse-identifier failed")
(assert-eq (parse-identifier "abc123") '("abc123" "") "parse-identifier failed")
(assert-eq (parse-identifier "a-b-c 123") '("a-b-c" " 123") "parse-identifier failed")
; -------------------------------------------------------------------------

(fn just (expected)
    (lambda (input)
      (let
        ((len (length expected))
        (actual (slice input 0 len)))
        (if (= expected actual)
          (list ':ok actual (slice input (length input)))
          (':err "no match found in 'just' parser" input)))))

(assert-eq ((just "abc") "abc 123") '("abc" " 123") "just failed")
