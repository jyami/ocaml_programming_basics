(* 目的:2以上の自然数のリストを受け取ったら2以上n以下の素数をすべて返す *)
(* sieve : int list -> int list *)

let rec sieve lst = match (lst) with
  [] -> []
  | first :: rest -> first :: sieve (List.filter (fun e -> e mod first > 0) rest)

let test0 = sieve [] = []
let test1 = sieve [2; 3; 4; 5;] = [2; 3; 5;]


let rec enumerate_n_to_2 n = if n = 1 then [] else n :: enumerate_n_to_2 (n-1)
let enumerate_2_to_n n = List.rev(enumerate_n_to_2 n)

let test2 = enumerate_2_to_n 5 = [2; 3; 4; 5;]


(* 目的:自然数を受け取ったらそれ以下の素数をすべて返す *)
(* prime : int -> int list *)

let prime n = sieve (enumerate_2_to_n n)

let test3 = prime 5 = [2; 3; 5;]
let test4 = prime 1 = []
let test5 = prime 2 = [2;]
