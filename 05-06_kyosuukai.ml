(* 2次方程式 ax2+bx+c=0の係数a,b,cを与えられたら虚数解かどうかを返す*)
(* hanbetsushiki = float->float->float->bool *)

let hanbetsushiki a b c = b *. b -. 4.0 *. a *. c

let test1 = hanbetsushiki 1.0 1.0 1.0 = -3.0
let test2 = hanbetsushiki 1.0 2.0 3.0 = -8.0

let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0.0
    then 2
    else if hanbetsushiki a b c = 0.0 then 1
    else 0

let test1 = kai_no_kosuu 1.0 2.0 1.0 = 1
let test2 = kai_no_kosuu 1.0 3.0 2.0 = 2
let test3 = kai_no_kosuu 1.0 1.0 1.0 = 0                      

let kyosuukai a b c = kai_no_kosuu a b c = 0

let test1 = kyosuukai 1.0 2.0 1.0 = false
let test2 = kyosuukai 1.0 3.0 2.0 = false
let test3 = kyosuukai 1.0 1.0 1.0 = true                       
