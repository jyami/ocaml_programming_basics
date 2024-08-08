(* 目的: 鶴と亀の数を与えられたら足の合計本数を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi tsuru kame =  tsuru_no_ashi tsuru + kame_no_ashi kame


let test1 = tsurukame_no_ashi 1 1 
let test2 = tsurukame_no_ashi 2 3
let test3 = tsurukame_no_ashi 4 2
