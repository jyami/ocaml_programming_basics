#use "09-09_global_ekimei_list.ml" 

(* ekimei_t型リスト受け取ったらそれをひらがな順に整列し、さらに駅の重複を取り除いたekimei_t型リストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)

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

let ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
]

let test0 = seiretsu [] = []
let test11 = seiretsu [{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; ] = [{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; ]
let test12 = seiretsu ekimei_list = [
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

