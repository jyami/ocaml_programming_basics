(* 整数のリストlstを受け取ったら、昇順に並べたリストを返す *)
(* ins_sort : int list -> int list *)

let rec insert lst n = match lst with
  [] -> [n]
  | first :: rest -> if n > first then first :: (insert rest n)
                                  else n :: first :: rest

let rec ins_sort lst = match lst with
  [] -> []
  | first :: rest -> insert (ins_sort rest) first

let test0 = ins_sort [] = []
let test1 = ins_sort [5] = [5]
let test2 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8]
