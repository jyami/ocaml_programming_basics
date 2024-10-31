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

(* ekimei_t型リストを受け取ったらその駅名を使ってeki_t型リストを作る *)
(* make_eki_list2 : ekimei_t list -> eki_t list *)

let make_eki_list2 lst = List.map (fun e -> match e with {kanji = kanji} -> {namae=kanji; saitan_kyori=infinity; temae_list=[]}) lst

let ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

let test0 = make_eki_list2 [] = []
let test1 = make_eki_list2 ekimei_list = [ 
{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}; 
{namae="代々木公園"; saitan_kyori=infinity; temae_list=[]}; 
]

