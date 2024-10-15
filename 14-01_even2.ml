(* 目的:整数のリストを受け取ったらの偶数のみのリストを返す。 *)
(* even : int list -> int list *)

let is_even t = (t mod 2) = 0

let rec even lst = List.filter is_even lst


let test1 = even [] = []
let test2 = even [1] = []
let test3 = even [2] = [2]
let test4 = even [2; 1; 6; 4; 7] = [2; 6; 4]
