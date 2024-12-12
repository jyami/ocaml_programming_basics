(* int -> int型の関数fとtree_t型の木を受け取ったら、節や葉に入っている値すべてにfを適用した木を返す *)
(* tree_map : int -> int -> tree_t -> tree_t *)

let rec tree_map f src = match src with
  Empty -> Empty
  | Leaf (n) -> Leaf (f n)
  | Node (l, n, r) -> Node ((tree_map f l), (f n), (tree_map f r))



let f n = n * 2

let t0 = tree_map f Empty = Empty
let t1 = tree_map f (Leaf (2)) = Leaf (4)
let t2 = tree_double (Node ((Leaf (2)), 3, (Node ((Leaf (5)), 6, (Leaf (7)))))) = Node ((Leaf (4)), 6, (Node ((Leaf (10)), 12, (Leaf (14)))))
