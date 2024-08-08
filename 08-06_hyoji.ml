type ekimei_t = {
    kanji : string;
    kana : string;
    romaji : string;
    shozoku : string
}

(* 目的:ekimei_tを受け取ったら「<路線名>, <かな>」を返す *)
(* hyoji : ekimei_t -> string *)
let hyoji e = match e with
  {kana = kana; kanji = kanji; shozoku = shozoku} ->
    shozoku ^ ", " ^ kanji ^ "(" ^ kana ^ ")"

let test1 = hyoji { kanji = "茗荷谷"; kana = "みょうがだに"; romaji = "myogadani"; shozoku = "丸の内線"} = "丸の内線, 茗荷谷(みょうがだに)"
