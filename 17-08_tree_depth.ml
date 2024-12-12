(* tree_t型の木を受け取ったら、気の深さを返す *)
(* tree_depth: tree_t -> int *)

let rec tree_depth src = 
  let rec hojo src depth = match src with
    Empty -> 0
    | Leaf (n) -> depth + 1
    | Node (l, n, r) -> (max (tree_depth l) (tree_depth r)) + 1
  in hojo src 0

let t0 = tree_depth Empty = 0
let t1 = tree_depth (Leaf (2)) = 1
let t2 = tree_depth (Node ((Leaf (2)), 3, (Node ((Leaf (5)), 6, (Leaf (7)))))) = 3
