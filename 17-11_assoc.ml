(* 目的:「駅名」と「駅名と距離の組のリスト」を受け取ったら、その駅までの距離を返す *)
(* assoc : string -> (string * float) list -> float *)

let assoc kanji_ekimei list = 
  let rec hojo l = match l with
    [] -> infinity
    | (first_name, first_kyori) :: rest -> 
      if first_name = kanji_ekimei then first_kyori
      else hojo rest
  in hojo list

let test0 = assoc "b" [] = infinity 
let test1 = assoc "b" [("a", 1.0); ("b", 2.0); ("c", 3.0)] = 2.0
let test2 = assoc "d" [("a", 1.0); ("b", 2.0); ("c", 3.0)] = infinity
