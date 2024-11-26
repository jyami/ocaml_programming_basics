(* 目的:関数fと初期値initとリストlstを受け取ったら、initからはじめてリストlstの要素を左から順にfに施しこむ *)
(* fold_left : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b *)

let fold_left f init lst = 
  let rec hojo result lst = match lst with
    [] -> result
    | first :: rest -> hojo (f result first) rest
in hojo init lst


let f = fun a b -> a ^ b
let test0 = fold_left f "" [] = ""
let test1 = fold_left f "a" ["b";"c";"d"] = "abcd"

(* 参考 *)
let test2 = List.fold_right f ["b";"c";"d"] "a" = "bcda"
let test3 = List.fold_left f "a" ["b";"c";"d"] = "abcd"
