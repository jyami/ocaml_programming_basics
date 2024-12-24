#use "17-12_insert_ekikan.ml"

(* ekikan_tree_t型の木とekikan_t list型の駅間リストを受け取ったらリストの中に含まれる駅間をすべて挿入した木を返す *)
(* inserts_ekikan : ekikan_tree_t -> ekikan_t list -> ekikan_tree_t *)

let inserts_ekikan tree list = List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan) list tree

let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 

let test0 = inserts_ekikan Empty [ekikan1; ekikan2; ekikan3]
