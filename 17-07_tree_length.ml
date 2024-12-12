(* tree_t型の木を受け取ったら、節と葉が合計いくつかるかを返す *)
(* tree_length: tree_t -> int *)

let rec tree_length src = 
  let rec hojo src sum = match src with
    Empty -> 0
    | Leaf (n) -> sum + 1
    | Node (l, n, r) -> sum + (tree_length l) + 1 + (tree_length r)
  in hojo src 0

let t0 = tree_length Empty = 0
let t1 = tree_length (Leaf (2)) = 1
let t2 = tree_length (Node ((Leaf (2)), 3, (Node ((Leaf (5)), 6, (Leaf (7)))))) = 5
