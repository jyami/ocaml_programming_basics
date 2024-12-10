#use "15-05_saitan_wo_bunri2.ml"

type ekimei_t = { 
  kanji   : string; (* 駅名 *) 
  kana    : string; (* 読み *) 
  romaji  : string; (* ローマ字 *) 
  shozoku : string; (* 所属線名 *) 
} 

type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}

type ekikan_t = {
    kiten : string;
    shuten : string;
    keiyu : string;
    kyori : float;
    jikan : int;
}

let make_eki_list2 lst = List.map (fun e -> match e with {kanji = kanji} -> {namae=kanji; saitan_kyori=infinity; temae_list=[]}) lst

let shokika2 lst kitenmei = List.map (fun e -> match e with 
  {namae=namae; saitan_kyori=saitan_kyori; temae_list=temae_list} ->
    if namae = kitenmei then {namae=namae; saitan_kyori=0.; temae_list=[namae]}
    else e) lst

(*
let saitan_wo_bunri2 lst = match lst with
  [] -> ({namae = "dummy"; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest -> List.fold_right
    (fun target (result, others) -> 
      if target.saitan_kyori <= result.saitan_kyori
      then (target, result :: others)
      else (result, target :: others)
    ) rest (first, [])
*)
let rec get_ekikan_kyori ekimei1 ekimei2 lst = match lst with
  [] -> infinity
  | first :: rest -> match first with
    {kiten = kiten; shuten = shuten; kyori = kyori} -> 
      if kiten = ekimei1 && shuten = ekimei2 then kyori
      else if kiten = ekimei2 && shuten = ekimei1 then kyori
      else get_ekikan_kyori ekimei1 ekimei2 rest

let koushin4 p v lst = let koushin1Withp q = (fun p q -> match p with
  { namae = pnamae; saitan_kyori = psaitan_kyori; temae_list = ptemae_list } -> match q with
    { namae = qnamae; saitan_kyori = qsaitan_kyori; temae_list = qtemae_list } -> 
    let kyori = get_ekikan_kyori pnamae qnamae lst in
      let p_keiyu_kyori = kyori +. psaitan_kyori in
        if kyori = infinity then q
        else if p_keiyu_kyori < qsaitan_kyori then
          { namae = qnamae; saitan_kyori = p_keiyu_kyori; temae_list = qnamae :: ptemae_list }
        else q) p q
  in List.map koushin1Withp v

(* 未確定の駅のリストvと駅間リストlstを受け取ったらダイクストラアルゴリズムに従って各駅について最短距離と最短経路が入ったリストを返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)

let sample_ekikan_list = [
  {kiten="A"; shuten="B"; keiyu=""; kyori=10.0; jikan=0};
  {kiten="A"; shuten="D"; keiyu=""; kyori=4.0; jikan=0};
  {kiten="B"; shuten="C"; keiyu=""; kyori=2.0; jikan=0};
  {kiten="B"; shuten="E"; keiyu=""; kyori=2.0; jikan=0}; 
  {kiten="C"; shuten="E"; keiyu=""; kyori=1.0; jikan=0}; 
  {kiten="D"; shuten="E"; keiyu=""; kyori=3.0; jikan=0}; 
]

let sample_ekimei_list = [ 
  {kanji="A"; kana="A"; romaji="A"; shozoku=""};
  {kanji="B"; kana="B"; romaji="B"; shozoku=""};
  {kanji="C"; kana="C"; romaji="C"; shozoku=""};
  {kanji="D"; kana="D"; romaji="D"; shozoku=""};
  {kanji="E"; kana="E"; romaji="E"; shozoku=""}; 
]


let rec dijkstra_main eki_list ekikan_list = match eki_list with
    [] -> []
    | first :: rest -> let (saitan_first, saitan_rest) = (saitan_wo_bunri2 (first :: rest))
        in let saitan_rest = koushin4 saitan_first saitan_rest ekikan_list
          in saitan_first :: dijkstra_main saitan_rest ekikan_list





let sample_eki_list = make_eki_list2 sample_ekimei_list
let v = shokika2 sample_eki_list "A"
let test0 = dijkstra_main v sample_ekikan_list = 
  [
    {namae = "A"; saitan_kyori = 0.; temae_list = ["A"]};
    {namae = "D"; saitan_kyori = 4.; temae_list = ["D"; "A"]};
    {namae = "E"; saitan_kyori = 7.; temae_list = ["E"; "D"; "A"]};
    {namae = "C"; saitan_kyori = 8.; temae_list = ["C"; "E"; "D"; "A"]};
    {namae = "B"; saitan_kyori = 9.; temae_list = ["B"; "E"; "D"; "A"]}
  ]
