(* 目的:2以上の自然数のリストを受け取ったら2以上n以下の素数をすべて返す *)
(* sieve2 : int list -> int list *)

let rec print_intlist l = match l with
  [] -> ()
  | first :: rest  -> print_int first; print_string ";"; print_intlist rest

let rec sieve2 lst = (print_int (List.length lst); print_string " : "; print_intlist lst; print_newline (); match lst with
  [] -> []
  | first :: rest -> first :: sieve2 (List.filter (fun e -> e mod first > 0) rest)
)

let test0 = sieve2 [] = []
let test1 = sieve2 [2; 3; 4; 5;] = [2; 3; 5;]
