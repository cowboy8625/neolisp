(struct Person
  name :string
  age :int)

(fn main ()
  (var people `(
    ,(Person:new :name "Bob" :age 20)))
  (var person (nth people 0))
  (var name (Person:get person :name))
  (var age (Person:get person :age))
  (print-fmt "name: {}, age: {}\n" name age)

  (var adding (list '+ 1 2))
  (var num 1)
  (var result `(,@adding ,num))
  (print-fmt "result: {}\n" result)
)
