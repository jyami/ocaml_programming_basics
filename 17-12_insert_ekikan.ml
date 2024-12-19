(* ekikan_tree_t型の木とekikan_t型の駅間を受け取ったら、その情報を挿入した木を返す *)
(* insert_ekikan : ekikan_tree_t -> ekikan_t -> ekikan_tree_t *)

type ekikan_tree_t = Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

type ekikan_t = {
    kiten : string;
    shuten : string;
    keiyu : string;
    kyori : float;
    jikan : int;
}

let insert_ekikan tree ekikan = match ekikan with
  {kiten=kiten; shuten=shuten; kyori=kyori} -> 
    let rec hojo tree2 eki1 eki2 = match tree2 with 
      Empty -> Node (Empty, eki1, [(eki2, kyori)], Empty)
      | Node (left, ekimei, list, right) ->
        if ekimei = eki1 then Node (left, ekimei, (eki2, kyori) :: list, right)
        else if ekimei < eki1 then Node (left, ekimei, list, (hojo right eki1 eki2))
        else Node ((hojo left eki1 eki2), ekimei, list, right)
    in hojo (hojo tree shuten kiten) kiten shuten

let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 
 
let tree1 = insert_ekikan Empty ekikan1 
let test1 = tree1 = 
  Node (Empty, "新大塚", [("池袋", 1.8)], 
	Node (Empty, "池袋", [("新大塚", 1.8)], Empty)) 
let tree2 = insert_ekikan tree1 ekikan2 
let test2 = tree2 = 
  Node (Empty, "新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
	Node (Empty, "池袋", [("新大塚", 1.8)], 
	      Node (Empty, "茗荷谷", [("新大塚", 1.2)], Empty))) 
let tree3 = insert_ekikan tree2 ekikan3 
let test3 = tree3 = 
  Node (Node (Empty, "後楽園", [("茗荷谷", 1.8)], Empty), 
	"新大塚", [("茗荷谷", 1.2); ("池袋", 1.8)], 
        Node (Empty, 
	      "池袋", [("新大塚", 1.8)], 
	      Node (Empty, 
		    "茗荷谷", [("後楽園", 1.8); ("新大塚", 1.2)], 
		    Empty))) 

