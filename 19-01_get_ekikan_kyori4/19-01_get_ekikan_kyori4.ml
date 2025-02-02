(* type ekikan_tree_t = Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t
*)

type ekikan_t = {
    kiten : string;
    shuten : string;
    keiyu : string;
    kyori : float;
    jikan : int;
}

(*
let rec search tree eki1 = match tree with
  Empty -> []
  | Node (left, shiten, list, right) -> if shiten = eki1 
    then list 
    else if shiten < eki1 then search right eki1
    else search left eki1
*)

(* exception Not_found *)

(* 漢字の駅名2つと駅間リスト受け取ったら2駅間の距離を返す *)
(* get_ekikan_kyori4 : string -> string -> ekikan_t lst -> float *)

let get_ekikan_kyori4 eki1 eki2 tree = let list = Tree.search tree eki1 in 
  let rec hojo l = match l with
    [] -> raise Not_found
    | (shuten, kyori) :: rest -> if eki2 = shuten
      then kyori
      else hojo rest
  in try hojo list with Not_found -> raise Not_found

(*
let insert_ekikan tree ekikan = match ekikan with
  {kiten=kiten; shuten=shuten; kyori=kyori} -> 
    let rec hojo tree2 eki1 eki2 = match tree2 with 
      Tree.empty -> Node (Tree.empty, eki1, [(eki2, kyori)], Tree.empty)
      | Node (left, ekimei, list, right) ->
        if ekimei = eki1 then Node (left, ekimei, (eki2, kyori) :: list, right)
        else if ekimei < eki1 then Node (left, ekimei, list, (hojo right eki1 eki2))
        else Node ((hojo left eki1 eki2), ekimei, list, right)
    in hojo (hojo tree shuten kiten) kiten shuten
*)

let insert_ekikan tree ekikan = match ekikan with
  {kiten=kiten; shuten=shuten; kyori=kyori} -> 
    let rec hojo tree2 eki1 eki2 kyori = 
      let lst = try Tree.search tree eki1 with
        Not_found -> []
      in Tree.insert tree2 eki1 ((eki2, kyori) :: lst)
    in hojo (hojo tree shuten kiten kyori) kiten shuten kyori

let inserts_ekikan tree list = List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan) list tree



(*
let test0 = get_ekikan_kyori4 "a" "b" Tree.empty
*)
(* ... Not_foundが起きる *)



let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 

let test1 = get_ekikan_kyori4 "新大塚" "茗荷谷" (inserts_ekikan Tree.empty [ekikan1; ekikan2; ekikan3]) = 1.2
let test2 = get_ekikan_kyori4 "後楽園" "茗荷谷" (inserts_ekikan Tree.empty [ekikan1; ekikan2; ekikan3]) = 1.8

