(* 目的:整数のリストを受け取ったらの偶数のみのリストを返す。 *)
(* length : int list -> int list *)
let rec even lst = match lst with
  [] -> []
  | first :: rest -> if is_even first then first::(even rest) else even rest

let is_even t = (t mod 2) = 0

let test1 = even [] = []
let test2 = even [1] = []
let test3 = even [2] = [2]
let test4 = even [2; 1; 6; 4; 7] = [2; 6; 4]
