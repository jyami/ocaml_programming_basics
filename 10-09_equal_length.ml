(* リストlst1,lst2を受け取ったら、それらの長さが同じかどうかを返す *)
(* equal_length : 'a list -> bool *)

let rec equal_length lst1 lst2 = match (lst1, lst2) with
  ([], []) -> true
  | ([], first2 :: rest2) -> false
  | (first1 :: rest1, []) -> false
  | (first1 :: [], first2 :: []) -> true
  | (first1 :: rest1, first2 :: rest2) -> equal_length rest1 rest2

let test0 = equal_length [] [] = true
let test1 = equal_length [] [1] = false
let test2 = equal_length [1] [] = false
let test3 = equal_length [1; 2] [3; 4] = true
let test4 = equal_length [1] [3; 4] = false
