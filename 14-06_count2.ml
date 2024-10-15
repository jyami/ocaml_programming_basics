type gakusei_t = {
    seiseki : string;
}

(* 目的:学生リストlstのうち成績が指定した成績の人の数を返す *)
(* count2 : gakusei_t list -> string -> int *)

let is_seiseki0 t seiseki = match t with {seiseki = s} -> s = seiseki 

let count2 lst seiseki = let is_seiseki t = is_seiseki0 t seiseki in length (List.filter is_seiseki lst)

let test0 = count2 [] "A" = 0
let test1 = count2 [{ seiseki = "A"}; { seiseki = "B"}; { seiseki = "A"}] "A" = 2
