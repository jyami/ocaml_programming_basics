#use "10-10_romaji_to_kanji.ml" 
#use "10-12_get_ekikan_kyori.ml" 

(* ローマ字の駅名2つ受け取ったら2駅間の距離を調べ「<駅名漢字>から<駅名漢字>までは○kmです」を返す *)
(* kyori_wo_hyoji : string -> string -> string *)

let rec kyori_wo_hyoji romaji_ekimei1 romaji_ekimei2 = 
  let kanji_ekimei1 = (romaji_to_kanji romaji_ekimei1 global_ekimei_list) in 
      if kanji_ekimei1 = "" then romaji_ekimei1 ^ "という駅は存在しません"
      else let kanji_ekimei2 = (romaji_to_kanji romaji_ekimei2 global_ekimei_list) in 
        if kanji_ekimei2 = "" then romaji_ekimei2 ^ "という駅は存在しません"
        else let kyori = (get_ekikan_kyori kanji_ekimei1 kanji_ekimei2 global_ekikan_list) in
          if kyori = infinity then kanji_ekimei1 ^ "駅と" ^ kanji_ekimei2 ^ "駅はつながっていません"
          else kanji_ekimei1 ^ "駅から" ^ kanji_ekimei2 ^ "駅までは" ^ string_of_float kyori ^ "kmです"


let test0 = kyori_wo_hyoji "shinotsuka" "hige" = "higeという駅は存在しません"
let test1 = kyori_wo_hyoji "hige" "shinotsuka" = "higeという駅は存在しません"
let test2 = kyori_wo_hyoji "shinotsuka" "korakuen" = "新大塚駅と後楽園駅はつながっていません"
let test3 = kyori_wo_hyoji "shinotsuka" "myogadani" = "新大塚駅から茗荷谷駅までは1.2kmです"
let test4 = kyori_wo_hyoji "korakuen" "myogadani" = "後楽園駅から茗荷谷駅までは1.8kmです"
