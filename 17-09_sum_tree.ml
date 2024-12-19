(* 多相の木を表す型 *)
type 'a tree_t = Empty (* 空の木 *)
  | Leaf of 'a (* 葉 *)
  | Node of 'a tree_t * 'a * 'a tree_t (* 節 *)

(* 目的:treeに含まれる整数をすべて加える *)
(* sum_tree : tree_t -> int *)
let rec sum_tree tree = match tree with
  Empty -> 0
  | Leaf (n) -> n
  | Node (t1, n, t2) -> sum_tree t1 + n + sum_tree t2


let t0 = sum_tree Empty = 0
let t1 = sum_tree (Leaf (2)) = 2
let t2 = sum_tree (Node ((Leaf (2)), 3, (Node ((Leaf (5)), 6, (Leaf (7)))))) = 23
