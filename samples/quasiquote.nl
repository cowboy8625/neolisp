(struct Person
  name :string
  age :int)

(fn main ()
  (var people `(
    ,(Person:new :name "Bob" :age 20)))
  (var person (nth people 0))
  (var name (Person:get person :name))
  (var age (Person:get person :age))
  (print-fmt "name: {}, age: {}\n" name age))
