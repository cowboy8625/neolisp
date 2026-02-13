;-----------------------------------
;         NOT WORKING YET
;-----------------------------------
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
  (var tokens '())
  (loop (not (= 0 (length input)))
    (print "input: '" (car input) "'\n")

    (if (letter? (car input))
      (let (output (parse-identifier input))
        (set tokens (append tokens (list (list :identifier (car output)))))
        (set input (cdr output)))

    (if (= " " (car input))
      (print "space\n")
      ; (set input (cdr input))

      ; Else
      (let (char (car input))
        (set input (cdr input))
        (set tokens (append tokens (list (list :unknown char))))))))
  tokens)

(test lexer (assert-eq :expected '((:identifier "abc")) :actual (lexer "abc") :description "lexer failed"))

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
; (assert-eq (parse-number "123abc") '("123" "abc") "parse-number failed")
; -------------------------------------------------------------------------

(fn letter? (expected)
    (let
      ((letters (split "" "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
      (truth-table (map (lambda (letter) (= letter expected)) letters)))
        (fold false (lambda (acc x) (or acc x)) truth-table)))

(test letter? (assert-eq :expected false :actual (letter? "1") :description "test letter? failed 1"))
; (assert-eq (letter? "(") false "test letter? failed 2")
; (assert-eq (letter? "z") true "test letter? failed 3")
; (assert-eq (letter? ">") false "test letter? failed 4")
; -------------------------------------------------------------------------

(fn punctuation? (expected)
  (let
    ((punctuations (split "" "!$,_-./:?+<=>#%&*@[\\]{|}`^~\"#;"))
    (truth-table (map (lambda (p) (= p expected)) punctuations)))
      (fold false (lambda (acc x) (or acc x)) truth-table)))

(test punctuation? (assert-eq :expected false :actual (punctuation? "1") :description "test punctuation? failed 1"))
; (assert-eq (punctuation? "(") false "test punctuation? failed 2")
; (assert-eq (punctuation? "z") false "test punctuation? failed 3")
; (assert-eq (punctuation? ">") true "test punctuation? failed 4")
; (assert-eq (punctuation? "\"") true "test punctuation? failed 5")
; -------------------------------------------------------------------------

(fn identifier? (input) (or (letter? input) (punctuation? input) (number? input)))

(test identifier?1 (assert-eq :expected true :actual (identifier? "-") :description "test is-identifier? function failed '-'"))
(test identifier?2 (assert-eq :expected true :actual (identifier? "1") :description "test is-identifier? function failed '1'"))
(test identifier?3 (assert-eq :expected true :actual (identifier? "?") :description "test is-identifier? function failed '?'"))
(test identifier?4 (assert-eq :expected true :actual (identifier? "a") :description "test is-identifier? function failed 'a'"))
; -------------------------------------------------------------------------

(fn parse-identifier (input)
  ; "accumulate the identifier in the input and return the parsed identifier and left over input"
  (let
    (identifier '())
      (loop (and (> (length input) 0) (identifier? (car input) (not (= " " (car input)))))
        (set identifier (append identifier (list (car input))))
        (set input (cdr input)))
      (list (join "" identifier)  input)))

(test parse-identifier (assert-eq :expected '("fn" " main ()") :actual (parse-identifier "fn main ()") :description "parse-identifier failed"))

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
; (assert-eq ((just "abc") "abc 123") '("abc" " 123") "just failed")
(fn main () (print (lexer "abc123 ()") "\n"))

; (fn main()
;   (var a (some 1))
;   (println a)
;   (println (some? a))
;   (println (none? a)))

