(* 目的:関数を二つ受け取ったらその二つの関数を合成した関数を返す *)
(* compose : ('a -> b') -> ('b -> 'c) -> ('a -> 'c) *)

let compose f1 f2 =
  let g x = f1 (f2 x)
  in g

let time2 x = x * 2
let add3 x = x + 3

let test = (compose time2 add3) 4
