
type gakusei_t = {
    tokuten : int;
}

(* 目的:gakusei_t型リストを受け取ったら、全員の得点の合計を返す *)
(* gakusei_sum : gakusei_t list -> int *)

let gakusei_tokuten_sum sum t = sum + t

let rec gakuseilst_2_tokutenlst lst = match lst with
  [] -> []
  | {tokuten=tokuten} :: rest -> tokuten :: gakuseilst_2_tokutenlst rest 

let gakusei_sum lst = List.fold_right gakusei_tokuten_sum (gakuseilst_2_tokutenlst lst) 0

let test0 = gakusei_sum [] = 0
let test1 = gakusei_sum [{ tokuten = 10}; { tokuten = 30}; { tokuten = 5}] = 45
