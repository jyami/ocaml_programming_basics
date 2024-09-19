#use "09-09_global_ekimei_list.ml" 
#use "12-01_eki_t.ml"

(* ekimei_t型リストを受け取ったらその駅名を使ってeki_t型リストを作る *)
(* makei_eki_list : ekimei_t list -> eki_t list *)

let rec makei_eki_list lst = match lst with
  [] -> []
  | first :: rest -> match first with
    {kanji = kanji} -> {namae=kanji; saitan_kyori=infinity; temae_list=[]} :: (makei_eki_list rest)

let ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

let test0 = makei_eki_list [] = []
let test1 = makei_eki_list ekimei_list = [ 
{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}; 
{namae="代々木公園"; saitan_kyori=infinity; temae_list=[]}; 
]
