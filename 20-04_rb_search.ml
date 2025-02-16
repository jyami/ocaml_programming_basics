#use "20-03_rb_insert.ml"  

(* 赤黒木とキーを受け取ったら、そのキーに対応する値を赤黒木の中から探して返す *)
(* rb_search : rb_tree_t -> 'a -> 'b *)

let rec rb_search tree key = match tree with
  Empty -> raise Not_found
  | Node (left, current_key, current_value, color, right) -> 
    if key = current_key then current_value
    else if current_key > key then rb_search left key
    else rb_search right key

let rb_tree0 = Empty 
(* let test0 = rb_search Empty 10 *)

let rb_tree1 = rb_insert rb_tree0 10 "x" 
let rb_tree2 = rb_insert rb_tree1 13 "y" 
let rb_tree3 = rb_insert rb_tree2 15 "z" 

let test1 = rb_search rb_tree3 10 = "x"
let test2 = rb_search rb_tree3 13 = "y"
let test3 = rb_search rb_tree3 15 = "z"
