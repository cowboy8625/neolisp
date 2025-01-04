;-----------------------------------
;         NOT WORKING YET
;-----------------------------------
(fn my-map (f lst)
  (if (= 0 (length lst))
    '()
    (cons (f (car lst))
      (my-map f (cdr lst)))))

(fn some (item) '(:some item))
(fn some? (item) (= :some (car item)))
(fn none () '(:none))
(fn none? (item) (= :none (car item)))
(fn ok (item) '(:ok item))
(fn ok? (item) (= :ok (car item)))
(fn err (item) '(:err))
(fn err? (item) (= :err (car item)))

(fn println (item) (print item "\n"))

(fn dbg (item)
  (print item "\n")
  item)


; input: String
(fn lexer (input)
  (var chars (split "" input))
  (var tokens '())
  (loop (not (= 0 (length chars)))
    (if (letter? (car chars))
      (set tokens (append tokens (parse-identifier chars)))
      (set tokens (append tokens '(:unknown (car chars))))
    )
  )
  tokens
)

; (test lexer (assert-eq :expected '((:identifier "abc")) :actual (lexer "abc") :description "lexer failed"))

(fn main()
  (var a (some 1))
  (println a)
  (println (some? a))
  (println (none? a))
)

; input List<char>
; (fn parse-number (input)
;   (let (number '())
;     (loop (and (not (= 0 (length input))) (number? (car input)))
;       (set number
;            (append
;              number
;              (list (nth input 0))))
;       (set input (cdr input))
;       (list (to-string number) input))))
;
; (test parse-number (assert-eq :expected '("123" "") :actual (parse-number "123") :description "parse-number failed"))
; ; (assert-eq (parse-number "123abc") '("123" "abc") "parse-number failed")
; ; -------------------------------------------------------------------------
;
(fn letter? (expected)
    (let
      ((letters (split "" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      (truth-table (my-map (lambda (letter) (= letter expected)) letters)))
        (fold false (lambda (acc x) (or acc x)) truth-table)))

(test letter? (assert-eq :expected false :actual (letter? "1") :description "test letter? failed 1"))
; (assert-eq (letter? "(") false "test letter? failed 2")
; (assert-eq (letter? "z") true "test letter? failed 3")
; (assert-eq (letter? ">") false "test letter? failed 4")
; -------------------------------------------------------------------------
;
(fn punctuation? (expected)
  (let
    ((punctuations (split "" "!$,_-./:?+<=>#%&*@[\\]{|}`^~\"#;")) ;"; this is just for my syntax highlighting to kick back in
    (truth-table (my-map (lambda (p) (= p expected)) punctuations)))
      (fold false (lambda (acc x) (or acc x)) truth-table)))

(test punctuation? (assert-eq :expected false :actual (punctuation? "1") "test punctuation? failed 1"))
; ; (assert-eq (punctuation? "(") false "test punctuation? failed 2")
; ; (assert-eq (punctuation? "z") false "test punctuation? failed 3")
; ; (assert-eq (punctuation? ">") true "test punctuation? failed 4")
; ; (assert-eq (punctuation? "\"") true "test punctuation? failed 5")
; ; -------------------------------------------------------------------------
;
(fn identifier? (input) (or (letter? input) (punctuation? input) (number? input)))

(test identifier? (assert-eq :expected true :actual (identifier? "-") :description "test is-identifier? function failed '-'"))
; ; (assert-eq (is-identifier? "1") true "test is-identifier? function failed '1'")
; ; (assert-eq (is-identifier? "?") true "test is-identifier? function failed '?'")
; ; (assert-eq (is-identifier? "a") true "test is-identifier? function failed 'a'")
; ; (assert-eq (is-identifier? " ") false "false is-identifier? function failed ' '")
; ; -------------------------------------------------------------------------
;
(fn parse-identifier (input)
  ; "accumulate the identifier in the input and return the parsed identifier and left over input"
  (let
    (identifier '())
      (loop (or (> 0 (length input)) (identifier? (car input)))
        (set identifier (append identifier (list (car input))))
        (set input (cdr input))
        (print "input='" input "'\n"))
      (list (to-string identifier) input)))

(test parse-identifier (assert-eq :expected '("a123457bc" "") :actual (parse-identifier "a123457bc") :description "parse-identifier failed"))
; (assert-eq (parse-identifier "abc") '("abc" "") "parse-identifier failed")
; (assert-eq (parse-identifier "abc123") '("abc123" "") "parse-identifier failed")
; (assert-eq (parse-identifier "a-b-c 123") '("a-b-c" " 123") "parse-identifier failed")
; -------------------------------------------------------------------------
;
; (fn just (expected)
;     (lambda (input)
;       (let
;         ((len (length expected))
;         (actual (slice input 0 len)))
;         (if (= expected actual)
;           (list ':ok actual (slice input (length input)))
;           (':err "no match found in 'just' parser" input)))))
;
; ; (assert-eq ((just "abc") "abc 123") '("abc" " 123") "just failed")
