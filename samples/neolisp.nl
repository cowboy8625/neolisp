(fn parse-number (input)
    "accumulate the number in the input and return the parsed number and left over input"
    ; This probably will change once concat is implemented
    (let
      ; variables
      ((number '())
      (is-running true))
      ; block
      (loop is-running
        (if
          ; condition: if the first element in input is a number
          (number? (car input))

          ; then append the number to the number
          (do
            ; asign new number to value
            (var number
                 ; append the number
                 (append
                   ; to the number
                    number
                   ; the number
                  (list (car input))))
            ; remove the first element
            ; input is returned
            (var input (cdr input))

            (if
              (= 0 (length input))
              ; then
              (var is-running false)
              ; else
              true))
          ; else return input
          (do
            (var is-running false)
            (list (to-string number) input))))))

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

(fn is-identifier? (input) (or (letter? input) (punctuation? input)))

(fn parse-identifier (input)
    "accumulate the identifier in the input and return the parsed identifier and left over input"
    ; This probably will change once concat is implemented
    (let
      ; variables
      ((identifier '())
      (is-running true))
      ; block
      (loop is-running
        (if
          ; condition: if the first element in input is a letter
          (is-identifier? (car input))
          ; then append the identifier to the identifier
          (do
            ; asign new identifier to value
            (var identifier
                 ; append the identifier
                 (append
                   ; to the identifier
                    identifier
                   ; the identifier
                  (list (car input))))
            ; remove the first element
            ; input is returned
            (var input (cdr input))
            (if
              (= 0 (length input))
              ; then
              (var is-running false)
              ; else
              true))
          ; else return input
          (do
            (var is-running false)
            (list (to-string identifier) input))))))


(assert-eq (parse-identifier "abc") '("abc" "") "parse-identifier failed")
(assert-eq (parse-identifier "abc123") '("abc" "123") "parse-identifier failed")
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
