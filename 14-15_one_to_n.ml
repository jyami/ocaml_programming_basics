(* 目的:1から受け取った自然数までの合計を返す。 *)
(* one_to_n : int -> int *)

let rec enumerate n = if n = 0 then [] else n :: enumerate (n-1)

let one_to_n n = List.fold_right (+) (enumerate n) 0

let test0 = one_to_n 10 = 55 = true
