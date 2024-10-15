#use "09-04_length.ml"

(* 目的:学籍リストlstのうち成績がAの人の数を返す *)
(* count_A2 : gakusei_t list -> int *)

type gakusei_t = {
    seiseki : string;
}

let is_seiseki_A t = match t with
  {seiseki = s} -> 
    if s = "A" then true
    else false

let count_A2 lst = length (List.filter is_seiseki_A lst)

let test0 = count_A2 [] = 0
let test1 = count_A2 [{ seiseki = "A"}; { seiseki = "B"}; { seiseki = "A"}] = 2
