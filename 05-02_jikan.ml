(* 目的:時間を受け取って、午前か午後化を返す *)
(*: jikan : int -> string *)
let jikan x = 
  if (x/12) mod 2 = 0 then "午前"
                   else "午後"  

let test1 = jikan 0 = "午前"
let test2 = jikan 12 = "午後"
let test3 = jikan 1 = "午前"
let test4 = jikan 13 = "午後" 
