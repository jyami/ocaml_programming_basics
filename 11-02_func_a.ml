(* func_a : int -> int *)

let rec func_a n =
  if n = 0 then 3
  else 2 * (func_a (n - 1)) - 1

let test0 = func_a 0 = 3
let test1 = func_a 1 = 5
let test2 = func_a 2 = 9 
