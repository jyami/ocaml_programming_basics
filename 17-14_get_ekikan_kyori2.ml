#use "17-13_inserts_ekikan.ml"

(* 漢字の駅名2つとekikan_tree_t型の木を受け取ってきたら、その2駅間の距離を返す *)
(* get_ekikan_kyori2 : string -> string -> ekikan_tree_t -> float *)


let rec search tree eki1 = match tree with
  Empty -> []
  | Node (left, shiten, list, right) -> if shiten = eki1 
    then list 
    else if shiten < eki1 then search right eki1
    else search left eki1

let get_ekikan_kyori2 eki1 eki2 tree = let list = search tree eki1 in 
  let rec hojo l = match l with
    [] -> infinity
    | (shuten, kyori) :: rest -> if eki2 = shuten
      then kyori
      else hojo rest
  in hojo list


let test0 = get_ekikan_kyori2 "a" "b" Empty = infinity

let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 

let test1 = get_ekikan_kyori2 "茗荷谷" "新大塚" (inserts_ekikan Empty [ekikan1; ekikan2; ekikan3]) = 1.2
