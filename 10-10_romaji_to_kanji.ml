#use "09-09_global_ekimei_list.ml" 

(* ローマ字の駅名と駅名リストを受け取ったらその駅の漢字表記を文字列で返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string *)

let rec romaji_to_kanji ekimei_romaji lst = match lst with
  [] -> ""
  | first :: rest -> match first with
    {romaji = romaji; kanji = kanji} -> 
      if romaji = ekimei_romaji then kanji
      else romaji_to_kanji ekimei_romaji rest

let test0 = romaji_to_kanji "hoge" [] = ""
let test1 = romaji_to_kanji "korakuen" global_ekimei_list = "後楽園"
let test2 = romaji_to_kanji "hoge" global_ekimei_list = ""
