(* 目的:2つの自然数mとnの最大公約数を返す *)
(* gcd : int -> int -> int *)

let rec gcd m n =
  if n = 0
  then m
  else gcd n (m mod n)

let test0 = gcd 10 0 = 10
let test1 = gcd 8 6 = 2
let test2 = gcd 36 24 = 12
