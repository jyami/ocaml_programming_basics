(* 目的:月、日を受け取って、星座を返す *)
(*: seiza : int -> int -> string *)
let seiza m d = 
  if (m = 3 && 21 <= d && d <= 31) || (m = 4 && 1 <= d && d <= 19) then "牡羊"
                   else if (m = 4 && 20 <= d && d <= 30) || (m = 5 && 1 <= d && d <= 20) then "牡牛"
                                                                                         else "other"

let test1 = seiza 3 21 = "牡羊"
let test2 = seiza 3 31 = "牡羊"
let test3 = seiza 4 19 = "牡羊"
let test4 = seiza 4 30 = "牡牛" 
let test5 = seiza 5 20 = "牡牛" 
