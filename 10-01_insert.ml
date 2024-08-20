(* 昇順に並んでいる整数のリストlstと整数nを受け取ったら、lstにnを追加して昇順に並べたリストを返す *)
(* insert : int list -> int -> int list *)

let rec insert lst n = match lst with
  [] -> [n]
  | first :: rest -> if n > first then first :: (insert rest n)
                                  else n :: first :: rest

let test0 = insert [] 1 = [1]
let test1 = insert [1] 2 = [1; 2] 
let test2 = insert [2] 1 = [1; 2] 
let test3 = insert [1; 3] 2 = [1; 2; 3]
let test4 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8]
