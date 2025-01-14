#use "09-09_global_ekimei_list.ml" 

(* exception No_such_station of string *)

(* ローマ字の駅名と駅名リストを受け取ったらその駅の漢字表記を文字列で返す *)
(* romaji_to_kanji2 : string -> ekimei_t list -> string *)

let rec romaji_to_kanji2 ekimei_romaji lst = match lst with
  [] -> raise (No_such_station (ekimei_romaji))
  | first :: rest -> match first with
    {romaji = romaji; kanji = kanji} -> 
      if romaji = ekimei_romaji then kanji
      else romaji_to_kanji2 ekimei_romaji rest

(* let test0 = romaji_to_kanji2 "hoge" [] *)
let test1 = romaji_to_kanji2 "korakuen" global_ekimei_list = "後楽園"
(* let test2 = romaji_to_kanji2 "hoge" global_ekimei_list *)

