open RedBlack

let rb_tree0 = empty
let rb_tree1 = insert rb_tree0 10 "x" 
let rb_tree2 = insert rb_tree1 13 "y" 
let rb_tree3 = insert rb_tree2 15 "z" 

let test01 = search rb_tree3 10 = "x"
let test02 = search rb_tree3 13 = "y"
let test03 = search rb_tree3 15 = "z"
