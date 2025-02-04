#use "20-01_rb_tree_t.ml"  

let balance tree = match tree with
  Node (Node (Node (a, xkey, xval, Red, b), ykey, yval, Red, c), zkey, zval, Black, d)
    | Node (Node (a, xkey, xval, Red, Node (b, ykey, yval, Red, c)), zkey, zval, Black, d)
      | Node (a, xkey, xval, Black, Node (Node (b, ykey, yval, Red, c), zkey, zval, Red, d))
        | Node (a, xkey, xval, Black, Node (b, ykey, yval, Red, Node (c, zkey, zval, Red, d)))
          -> Node (Node (a, xkey, xval, Black, b), ykey, yval, Red, Node (c, zkey, zval, Black, d))
  | _ -> tree

(* 赤黒木とキーと値を受け取ったら、それを挿入した赤黒木を返す *)
(* rb_insert : rb_tree_t -> 'a -> 'b -> rb_tree_t *)

let rec rb_insert2 tree key value = match tree with
  Empty -> Node (Empty, key, value, Red, Empty) (* 木が空だったら頂点を置き換える。色は赤とする。 *)
  | Node (left, current_key, current_value, color, right) -> 
    if current_key = key then Node (left, key, value, color, right)
    else if current_key > key then balance (Node ((rb_insert2 left key value), current_key, current_value, color, right))
    else balance (Node (left ,current_key, current_value, color, (rb_insert2 right key value)))

let rb_insert tree key value = match (rb_insert2 tree key value) with
    Empty -> assert false (* 起こりえない *)
    | Node (left, current_key, current_value, color, right) -> Node (left, current_key, current_value, Black, right) (* `最後に返ってきた木の根の色を黒にする` *)

let rb_tree0 = Empty 
let rb_tree1 = rb_insert rb_tree0 10 "x" 
let rb_tree2 = rb_insert rb_tree1 13 "y" 
let rb_tree3 = rb_insert rb_tree2 15 "z" 
 
let test1 = rb_tree1 = Node (Empty, 10, "x", Black, Empty) 
let test2 = rb_tree2 = Node (Empty, 10, "x", Black, 
			     Node (Empty, 13, "y", Red, Empty)) 
let test3 = rb_tree3 = Node (Node (Empty, 10, "x", Black, Empty), 
			     13, "y", Black, 
			     Node (Empty, 15, "z", Black, Empty)) 

