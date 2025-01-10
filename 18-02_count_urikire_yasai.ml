let rec price item yaoya_list = match yaoya_list with
  [] -> None
  | (yasai, nedan) :: rest -> if item = yasai
      then Some (nedan)
      else price item rest

(* 目的: 野菜のリストと八百屋のリストを受け取ったら、野菜のリストのうち八百屋に置いていない野菜の数を返す *)
(* count_urikire_yasai: string list -> (string * int) list -> int *)

let count_urikire_yasai yasai_list yaoya_list = 
  let rec hojo yasai_list yaoya_list init =
    match yasai_list with
    [] -> init
    | first :: rest -> match price first yaoya_list with
        None -> hojo rest yaoya_list init + 1
        | Some (p) -> hojo rest yaoya_list init
  in hojo yasai_list yaoya_list 0

let test0 = count_urikire_yasai [] [] = 0
let test1 = count_urikire_yasai ["トマト"] [] = 1
let test2 = count_urikire_yasai [] [("キャベツ",400); ("トマト",300)] = 0
let test3 = count_urikire_yasai ["トマト"] [("トマト",300)] = 0
let test4 = count_urikire_yasai ["トマト"; "きゅうり"; "白菜"] [("キャベツ",400); ("トマト",300)] = 2
