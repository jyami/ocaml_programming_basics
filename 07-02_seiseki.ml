(* seiseki = string * string -> string *)

let seiseki pair = match pair with
  (a, b) -> a ^ "さんの評価は" ^ b ^ "です"

let test1 = seiseki ("eiji", "A")
