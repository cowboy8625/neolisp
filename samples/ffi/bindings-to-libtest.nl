(ffi-bind
  :library "samples/ffi/libtest"
  :symbol  "add"
  :fn      libtest.add
  :args    (int int)
  :return  int)

(ffi-bind
  :library "samples/ffi/libtest"
  :symbol  "sub"
  :fn      libtest.sub
  :args    (int int)
  :return  int)

(ffi-bind
  :library "samples/ffi/libtest"
  :symbol  "neolisp_printf"
  :fn      printf
  ; currently no support for varargs :(
  :args    (string int int int string)
  :return  nil)

(ffi-bind
  :library "samples/ffi/libtest"
  :symbol  "neolisp_input"
  :fn      input
  :args    ()
  :return  string)

(fn main ()
  (var v (libtest.add 123 321))
  (var in (input))
  (printf "'%d %d %d %s'\n" v 2 3 in))
