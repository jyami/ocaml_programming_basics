(* 赤か黒を示す型 *)
type color_t = Red | Black

(* 赤黒木を表す型 *)
type ('a, 'b) t = 
  Empty
  | Node of ('a, 'b) t * 'a * 'b * color_t * ('a, 'b) t

(* 空の赤黒木 *) 
let empty = Empty 

(* t型の木を受け取ったらバランスしているかを調べ、バランスしていなければバランスして返す *)
(* balance : ('a, 'b) t -> ('a, 'b) t *)

let balance tree = match tree with
  Node (Node (Node (a, xkey, xval, Red, b), ykey, yval, Red, c), zkey, zval, Black, d)
    | Node (Node (a, xkey, xval, Red, Node (b, ykey, yval, Red, c)), zkey, zval, Black, d)
      | Node (a, xkey, xval, Black, Node (Node (b, ykey, yval, Red, c), zkey, zval, Red, d))
        | Node (a, xkey, xval, Black, Node (b, ykey, yval, Red, Node (c, zkey, zval, Red, d)))
          -> Node (Node (a, xkey, xval, Black, b), ykey, yval, Red, Node (c, zkey, zval, Black, d))
  | _ -> tree


(* 赤黒木とキーと値を受け取ったら、それを挿入した赤黒木を返す *)
(* insert : tree_t -> 'a -> 'b -> tree_t *)

let rec insert2 tree key value = match tree with
  Empty -> Node (Empty, key, value, Red, Empty) (* 木が空だったら頂点を置き換える。色は赤とする。 *)
  | Node (left, current_key, current_value, color, right) -> 
    if current_key = key then Node (left, key, value, color, right)
    else if current_key > key then balance (Node ((insert2 left key value), current_key, current_value, color, right))
    else balance (Node (left ,current_key, current_value, color, (insert2 right key value)))

let insert tree key value = match (insert2 tree key value) with
    Empty -> assert false (* 起こりえない *)
    | Node (left, current_key, current_value, color, right) -> Node (left, current_key, current_value, Black, right) (* `最後に返ってきた木の根の色を黒にする` *)


(* 赤黒木とキーを受け取ったら、そのキーに対応する値を赤黒木の中から探して返す *)
(* search : t -> 'a -> 'b *)

let rec search tree key = match tree with
  Empty -> raise Not_found
  | Node (left, current_key, current_value, color, right) -> 
    if key = current_key then current_value
    else if current_key > key then search left key
    else search right key
