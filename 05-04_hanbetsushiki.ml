(* 2次方程式 ax2+bx+c=0の係数a,b,cを与えられたら判別式の値を返す*)
(* hanbetsushiki = float->float->float->float *)

let hanbetsushiki a b c = b *. b -. 4.0 *. a *. c

let test1 = hanbetsushiki 1.0 1.0 1.0 = -3.0
let test2 = hanbetsushiki 1.0 2.0 3.0 = -8.0
