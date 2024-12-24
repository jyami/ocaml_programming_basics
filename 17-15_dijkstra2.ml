#use "09-09_global_ekimei_list.ml" 
#use "09-10_global_ekikan_list.ml"

type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}

let rec insert_ekimei lst tgt = match lst with
  [] -> [tgt]
  | first :: rest -> 
    match first with {kana=fkana} -> 
      match tgt with {kana=tkana} ->
        if tkana > fkana then first :: (insert_ekimei rest tgt)
        else if tkana = fkana then tgt::rest
        else tgt :: first :: rest

let rec seiretsu lst = match lst with
  [] -> []
  | first :: rest -> insert_ekimei (seiretsu rest) first


let rec romaji_to_kanji ekimei_romaji lst = match lst with
  [] -> ""
  | first :: rest -> match first with
    {romaji = romaji; kanji = kanji} -> 
      if romaji = ekimei_romaji then kanji
      else romaji_to_kanji ekimei_romaji rest

let make_initial_eki_list lst kitenmei = List.map (fun e -> match e with {kanji=namae} ->
    if namae = kitenmei then {namae=namae; saitan_kyori=0.; temae_list=[namae]}
    else {namae=namae; saitan_kyori=infinity; temae_list=[]}
    ) lst 

let saitan_wo_bunri2 lst = match lst with
  [] -> ({namae = "dummy"; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest -> List.fold_right
    (fun target (result, others) -> 
      if target.saitan_kyori <= result.saitan_kyori
      then (target, result :: others)
      else (result, target :: others)
    ) rest (first, [])



type ekikan_tree_t = Empty
  | Node of ekikan_tree_t * string * (string * float) list * ekikan_tree_t

let insert_ekikan tree ekikan = match ekikan with
  {kiten=kiten; shuten=shuten; kyori=kyori} -> 
    let rec hojo tree2 eki1 eki2 = match tree2 with 
      Empty -> Node (Empty, eki1, [(eki2, kyori)], Empty)
      | Node (left, ekimei, list, right) ->
        if ekimei = eki1 then Node (left, ekimei, (eki2, kyori) :: list, right)
        else if ekimei < eki1 then Node (left, ekimei, list, (hojo right eki1 eki2))
        else Node ((hojo left eki1 eki2), ekimei, list, right)
    in hojo (hojo tree shuten kiten) kiten shuten

let inserts_ekikan tree list = List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan) list tree

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

(*
let rec get_ekikan_kyori ekimei1 ekimei2 lst = match lst with
  [] -> infinity
  | first :: rest -> match first with
    {kiten = kiten; shuten = shuten; kyori = kyori} -> 
      if kiten = ekimei1 && shuten = ekimei2 then kyori
      else if kiten = ekimei2 && shuten = ekimei1 then kyori
      else get_ekikan_kyori ekimei1 ekimei2 rest
*)

let koushin4 p v ekikan_tree = let koushin1Withp q = (fun p q -> match p with
  { namae = pnamae; saitan_kyori = psaitan_kyori; temae_list = ptemae_list } -> match q with
    { namae = qnamae; saitan_kyori = qsaitan_kyori; temae_list = qtemae_list } -> 
    let kyori = get_ekikan_kyori2 pnamae qnamae ekikan_tree in
      let p_keiyu_kyori = kyori +. psaitan_kyori in
        if kyori = infinity then q
        else if p_keiyu_kyori < qsaitan_kyori then
          { namae = qnamae; saitan_kyori = p_keiyu_kyori; temae_list = qnamae :: ptemae_list }
        else q) p q
  in List.map koushin1Withp v

let rec dijkstra_main2 eki_list ekikan_tree = match eki_list with
    [] -> []
    | first :: rest -> let (saitan_first, saitan_rest) = (saitan_wo_bunri2 (first :: rest))
        in let saitan_rest = koushin4 saitan_first saitan_rest ekikan_tree
          in saitan_first :: dijkstra_main2 saitan_rest ekikan_tree


(* 始点の駅名(ローマ字)と終点の駅名(ローマ字)を受け取ったら最短経路をもとめ、終点駅のレコードを返す *)
(* dijkstra: string -> string -> eki_t *)

let dijkstra shitenEkimei shutenEkimei = 
  let uniqueEkimeiList = seiretsu global_ekimei_list
    in let shitenKanji = romaji_to_kanji shitenEkimei uniqueEkimeiList
      in let shutenKanji = romaji_to_kanji shutenEkimei uniqueEkimeiList
        in let ekiList = make_initial_eki_list uniqueEkimeiList shitenKanji
          in let ekikan_tree = inserts_ekikan Empty global_ekikan_list
          in List.filter (fun e -> e.namae = shutenKanji) (dijkstra_main2 ekiList ekikan_tree)

let test0 = dijkstra "tawaramachi" "ueno"
let test1 = dijkstra "ueno" "tawaramachi" 
let test2 = dijkstra "shibuya" "gokokuji"
let test3 = dijkstra "myogadani" "meguro"
