(* 目的:文字列のリストを受け取ったらの全部連結した文字列を返す。 *)
(* concat2 : string list -> string *)

let join_str sum s = sum ^ s

let concat2 lst = List.fold_right join_str lst ""

let test1 = concat2 ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
