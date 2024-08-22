type gakusei_t = {
    tensuu : int;
}

(* gakusei_t型リストlstを受け取ったら、それをtensuuフィールド昇順に並べたリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)

let rec insert lst g = match lst with
  [] -> [g]
  | first :: rest -> match g with
    {tensuu = gt} -> match first with 
      {tensuu = ft} -> if gt > ft then first :: (insert rest g)
                                  else g :: first :: rest

let rec gakusei_sort lst = match lst with
  [] -> []
  | first :: rest -> insert (gakusei_sort rest) first

let test0 = gakusei_sort [] = []
let test1 = gakusei_sort [{tensuu=5}] = [{tensuu=5}]
let test2 = gakusei_sort [{tensuu=5}; {tensuu=3}; {tensuu=8}; {tensuu=1}; {tensuu=7}; {tensuu=4}] = [{tensuu=1}; {tensuu=3}; {tensuu=4}; {tensuu=5}; {tensuu=7}; {tensuu=8}]
