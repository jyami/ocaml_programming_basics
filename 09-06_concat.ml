(* 目的:文字列のリストを受け取ったらの全部連結した文字列を返す。 *)
(* concat : string list -> string *)
let rec concat lst = match lst with
  [] -> ""
  | first :: rest -> first ^ (concat rest)

let test1 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬"
