type gakusei_t = {
    tensuu : int;
}

(* gakusei_t型リストlstを受け取ったら、tensuuフィールドが最大の要素を返す *)
(* gakusei_max : gakusei_t list -> gakusei_t *)

let rec gakusei_max lst = match lst with
  [] -> {tensuu=min_int}
  | first :: rest -> match first with
    {tensuu=ft} -> 
      match(gakusei_max rest) with 
        {tensuu=mt} ->
          if ft >= mt then first
                      else gakusei_max rest

let test0 = gakusei_max [] = {tensuu=min_int}
let test1 = gakusei_max [{tensuu=5}] = {tensuu=5}
let test2 = gakusei_max [{tensuu=5}; {tensuu=3}; {tensuu=8}; {tensuu=1}; {tensuu=7}; {tensuu=4}] = {tensuu=8}
