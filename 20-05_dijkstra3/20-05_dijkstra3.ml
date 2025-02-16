#use "../09-09_global_ekimei_list.ml" 
#use "../09-10_global_ekikan_list.ml"

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




(* 漢字の駅名2つと駅間リスト受け取ったら2駅間の距離を返す *)
(* get_ekikan_kyori5 : string -> string -> ekikan_t lst -> float *)
let get_ekikan_kyori5 eki1 eki2 tree = let list = RedBlack.search tree eki1 in 
  let rec hojo l = match l with
    [] -> raise Not_found
    | (shuten, kyori) :: rest -> (
        (* print_string "eki2 = "; print_string shuten; print_newline (); *)
        if eki2 = shuten
        then kyori
        else hojo rest
      )
  in try hojo list with Not_found -> raise Not_found

let insert_ekikan tree ekikan = match ekikan with
  {kiten=kiten; shuten=shuten; kyori=kyori} -> 
    let rec hojo tree2 eki1 eki2 kyori = 
      let lst = try RedBlack.search tree eki1 with
        Not_found -> []
      in RedBlack.insert tree2 eki1 ((eki2, kyori) :: lst)
    in hojo (hojo tree shuten kiten kyori) kiten shuten kyori

let inserts_ekikan tree list = List.fold_right (fun ekikan tree -> insert_ekikan tree ekikan) list tree


let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 

let test11 = get_ekikan_kyori5 "新大塚" "茗荷谷" (inserts_ekikan RedBlack.empty [ekikan1; ekikan2; ekikan3]) = 1.2
let test12 = get_ekikan_kyori5 "後楽園" "茗荷谷" (inserts_ekikan RedBlack.empty [ekikan1; ekikan2; ekikan3]) = 1.8



let koushin5 p v ekikan_tree = let koushin1Withp q = try (fun p q -> match p with
  { namae = pnamae; saitan_kyori = psaitan_kyori; temae_list = ptemae_list } -> match q with
    { namae = qnamae; saitan_kyori = qsaitan_kyori; temae_list = qtemae_list } -> 
    let kyori = get_ekikan_kyori5 pnamae qnamae ekikan_tree in
      let p_keiyu_kyori = kyori +. psaitan_kyori in
        if kyori = infinity then q
        else if p_keiyu_kyori < qsaitan_kyori then
          { namae = qnamae; saitan_kyori = p_keiyu_kyori; temae_list = qnamae :: ptemae_list }
        else q) p q with Not_found -> q
  in List.map koushin1Withp v

let ekikan1 = 
  {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8; jikan=3} 
let ekikan2 = 
  {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2; jikan=2} 
let ekikan3 = 
  {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8; jikan=2} 

let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
let test21 = koushin5 eki2 [] (inserts_ekikan RedBlack.empty [ekikan1; ekikan2; ekikan3]) = [] 
let test22 = koushin5 eki2 [eki1; eki2; eki3; eki4] (inserts_ekikan RedBlack.empty [ekikan1; ekikan2; ekikan3]) = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 




let rec dijkstra_main2 eki_list ekikan_tree = match eki_list with
    [] -> []
    | first :: rest -> let (saitan_first, saitan_rest) = (saitan_wo_bunri2 (first :: rest))
        in let saitan_rest = koushin5 saitan_first saitan_rest ekikan_tree
          in saitan_first :: dijkstra_main2 saitan_rest ekikan_tree


(* 始点の駅名(ローマ字)と終点の駅名(ローマ字)を受け取ったら最短経路をもとめ、終点駅のレコードを返す *)
(* dijkstra3: string -> string -> eki_t *)

let dijkstra3 shitenEkimei shutenEkimei = 
  let uniqueEkimeiList = seiretsu global_ekimei_list
    in let shitenKanji = romaji_to_kanji shitenEkimei uniqueEkimeiList
      in let shutenKanji = romaji_to_kanji shutenEkimei uniqueEkimeiList
        in let ekiList = make_initial_eki_list uniqueEkimeiList shitenKanji
          in let ekikan_tree = inserts_ekikan RedBlack.empty global_ekikan_list
          in List.filter (fun e -> e.namae = shutenKanji) (dijkstra_main2 ekiList ekikan_tree)

(* for debug
let uniqueEkimeiList = seiretsu global_ekimei_list
let shitenKanji = romaji_to_kanji "shibuya" uniqueEkimeiList
let shutenKanji = romaji_to_kanji "gokokuji" uniqueEkimeiList
let ekiList = make_initial_eki_list uniqueEkimeiList shitenKanji
let ekikan_tree = inserts_ekikan RedBlack.empty global_ekikan_list
*)

let test30 = dijkstra3 "tawaramachi" "ueno"
let test31 = dijkstra3 "ueno" "tawaramachi"
let test32 = dijkstra3 "shibuya" "gokokuji"
let test33 = dijkstra3 "myogadani" "meguro"
