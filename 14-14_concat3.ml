(* 目的:文字列のリストを受け取ったらの全部連結した文字列を返す。 *)
(* concat3 : string list -> string *)

let concat3 lst = List.fold_right (^) lst ""

let test1 = concat3 ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
