(* 2次方程式 ax2+bx+c=0の係数a,b,cを与えられたら解の個数を返す*)
(* hanbetsushiki = float->float->float->int *)

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
