(* 目的:「駅名」と「駅名と距離の組のリスト」を受け取ったら、その駅までの距離を返す *)
(* assoc2 : string -> (string * float) list -> float *)

exception Not_found

let assoc2 kanji_ekimei list = 
  let rec hojo l = match l with
    [] -> raise Not_found
    | (first_name, first_kyori) :: rest -> 
      if first_name = kanji_ekimei then first_kyori
      else hojo rest
  in hojo list

let test0 = assoc2 "b" [] (* 例外が起きる *)
let test1 = assoc2 "b" [("a", 1.0); ("b", 2.0); ("c", 3.0)] = 2.0
let test2 = assoc2 "d" [("a", 1.0); ("b", 2.0); ("c", 3.0)] (* 例外が起きる *)
