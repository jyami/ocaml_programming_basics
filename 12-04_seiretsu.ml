#use "09-09_global_ekimei_list.ml" 

(* ekimei_t型リスト受け取ったらそれをひらがな順に整列し、さらに駅の重複を取り除いたekimei_t型リストを返す *)
(* seiretsu : ekimei_t list -> ekimei_t list *)

let rec insert_sort_ekimei lst tgt = match lst with
  [] -> [tgt]
  | first :: rest -> 
    match first with {kana=fkana} -> 
      match tgt with {kana=tkana} ->
        if tkana > fkana then first :: (insert_sort_ekimei rest tgt)
        else tgt :: first :: rest

let rec sort_ekimei lst = match lst with
  [] -> []
  | first :: rest -> insert_sort_ekimei (sort_ekimei rest) first

let rec seiretsu2 lst = match lst with
  [] -> []
  | first :: rest1 -> match rest1 with
    [] -> [first]
    | second :: rest2 -> 
      match first with {kana=fkana} -> 
        match second with {kana=skana} ->
          if fkana = skana then first :: (seiretsu2 rest2) (* secondを外した *)
          else first :: (seiretsu2 rest1)

let seiretsu lst = seiretsu2 (sort_ekimei lst)


let ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
]

let test0 = seiretsu [] = []
let test1 = sort_ekimei ekimei_list = [ 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="丸ノ内線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]
let test11 = seiretsu [{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; ] = [{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; ]
let test12 = seiretsu ekimei_list = [
{kanji="池袋"; kana="いけぶくろ"; romaji="ikebukuro"; shozoku="有楽町線"}; 
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijinguumae"; shozoku="千代田線"}; 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

