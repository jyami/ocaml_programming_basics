(* 目的: 受け取った数値と数値のリストの中の最小値を返す。 *)
(* minimum2 : int -> int list -> int *)

let rec minimum2 kouho_first kouho_rest = match kouho_rest with
  [] -> kouho_first
  | first :: rest -> let min = minimum2 first rest 
    in if kouho_first <= min
      then kouho_first
      else min

let test0 = minimum2 1 [] = 1
let test1 = minimum2 1 [2; 3;] = 1
let test2 = minimum2 2 [1; 3;] = 1
let test3 = minimum2 3 [2; 1;] = 1
