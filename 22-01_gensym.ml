let count = ref 0

let gensym s = (count := !count + 1;
  s ^ (string_of_int (!count-1))
)

let test0 = gensym "a" = "a0"
let test1 = gensym "xyz" = "xyz1"
