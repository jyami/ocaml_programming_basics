(* 目的: 鶴と亀の数の合計と足の合計を与えられたら鶴の数を返す *)
(* tsurukame : int -> int -> int *)
let tsurukame hiki ashi = 2 * hiki - ashi / 2 

(*
t + k = hiki
2t + 4k = ashi

k = hiki - t
2t + 4(hiki - t) = ashi
2t - 4t + 4hiki = ashi
-2t = ashi - 4hiki
t = 2 * hiki - ashi / 2 
*)

let test1 = tsurukame 2 6 (* kame 1 tsuru 1 *) 
let test2 = tsurukame 3 8 (* kame 1 tsuru 2 *)
let test3 = tsurukame 3 10 (* kame 2 tsuru 1 *)
