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

(* ekimei_t型リストと起点名を受け取ったら、起点のみsaitankyoriは0、temae_listは始点駅名からのみなるリストを持つeki_t型リストを返す *)
(* make_initial_eki_list : ekimei_t list -> -> string -> eki_t list *)

let make_initial_eki_list lst kitenmei = List.map (fun e -> match e with {kanji=namae} ->
    if namae = kitenmei then {namae=namae; saitan_kyori=0.; temae_list=[namae]}
    else {namae=namae; saitan_kyori=infinity; temae_list=[]}
    ) lst 

let ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

let test0 = make_initial_eki_list [] "代々木公園" = []
let test1 = make_initial_eki_list ekimei_list "代々木公園" = [ 
{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}; 
{namae="代々木公園"; saitan_kyori=0.; temae_list=["代々木公園"]}; 
]
