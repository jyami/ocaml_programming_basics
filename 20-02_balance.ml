#use "20-01_rb_tree_t.ml" 

(* rb_tree_t型の木を受け取ったらバランスしているかを調べ、バランスしていなければバランスして返す *)
(* balance : ('a, 'b) rb_tree_t -> ('a, 'b) rb_tree_t *)

let balance tree = match tree with
  Node (Node (Node (a, xkey, xval, Red, b), ykey, yval, Red, c), zkey, zval, Black, d)
    | Node (Node (a, xkey, xval, Red, Node (b, ykey, yval, Red, c)), zkey, zval, Black, d)
      | Node (a, xkey, xval, Black, Node (Node (b, ykey, yval, Red, c), zkey, zval, Red, d))
        | Node (a, xkey, xval, Black, Node (b, ykey, yval, Red, Node (c, zkey, zval, Red, d)))
          -> Node (Node (a, xkey, xval, Black, b), ykey, yval, Red, Node (c, zkey, zval, Black, d))
  | _ -> tree

let rb_tree1 = 
  Node (Node (Node (Empty, 10, "x", Red, Empty), 13, "y", Red, Empty), 
	15, "z", Black, Empty) 
let rb_tree2 = 
  Node (Node (Empty, 10, "x", Red, Node (Empty, 13, "y", Red, Empty)), 
	15, "z", Black, Empty) 
let rb_tree3 = 
  Node (Empty, 10, "x", Black, 
	Node (Node (Empty, 13, "y", Red, Empty), 15, "z", Red, Empty)) 
let rb_tree4 = 
  Node (Empty, 10, "x", Black, 
	Node (Empty, 13, "y", Red, Node (Empty, 15, "z", Red, Empty))) 
let rb_tree5 = 
  Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, 
	Node (Empty, 15, "z", Black, Empty)) 
let rb_tree6 = Empty 
let test1 = balance rb_tree1 = rb_tree5 
let test2 = balance rb_tree2 = rb_tree5 
let test3 = balance rb_tree3 = rb_tree5 
let test4 = balance rb_tree4 = rb_tree5 
let test5 = balance rb_tree6 = rb_tree6 
