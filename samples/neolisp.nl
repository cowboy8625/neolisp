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

(print (parse-number "123abc") "\n")

(assert-eq (parse-number "123") '("123" "") "parse-number failed")
(assert-eq (parse-number "123abc") '("123" "abc") "parse-number failed")
