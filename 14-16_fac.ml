(* 目的:受け取った自然数の階乗を返す。 *)
(* fac : int -> int *)

let rec enumerate n = if n = 0 then [] else n :: enumerate (n-1)

let fac n = List.fold_right ( * ) (enumerate n) 1

let test0 = fac 3 = 6 = true
let test1 = fac 5 = 120 = true
let test2 = fac 1 = 1 = true
