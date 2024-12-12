(* tree_t型の木を受け取ったら、節や葉に入っている値を2倍にして返す。 *)
(* tree_double: tree_t -> tree_t *)

type tree_t = Empty
  | Leaf of int
  | Node of tree_t * int * tree_t

let rec tree_double src = match src with
  Empty -> Empty
  | Leaf (n) -> Leaf (n * 2)
  | Node (t1, n, t2) -> Node ((tree_double t1), (n * 2), (tree_double t2))


let t0 = tree_double Empty = Empty
let t1 = tree_double (Leaf (2)) = Leaf (4)
let t2 = tree_double (Node ((Leaf (2)), 3, (Node ((Leaf (5)), 6, (Leaf (7)))))) = Node ((Leaf (4)), 6, (Node ((Leaf (10)), 12, (Leaf (14)))))
