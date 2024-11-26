(* 目的:整数のリストを受け取ったらそれまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list *)

let sum_list lst =
  let rec hojo lst sum = match lst with
    [] -> []
    | first :: rest -> 
      let sub = first + sum in sub :: hojo rest sub
  in hojo lst 0


let test0 = sum_list [] = []
let test1 = sum_list [3; 2; 1; 4] = [3; 5; 6; 10]
