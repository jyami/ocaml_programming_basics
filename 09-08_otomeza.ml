(* person_t型のデータリストを受け取ったら、おとめ座の人の名前のみからなるリストを返す *)
(* otomeza : person_t list -> string list *)

type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

let rec otomeza lst = match lst with
  [] -> []
  | {birthday = bd; name = n} :: rest -> match bd with
    (m, d) -> if (m = 8 && d >= 23 && d <= 31) || (m = 9 && d >= 1 && d <= 22)
      then n::(otomeza rest) 
      else otomeza rest
    
let test0 = otomeza [] = []
let test1 = otomeza [{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (8,23); blood = "A"}] = ["hoge"]
let test2 = otomeza [{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (8,23); blood = "A"};
             { name = "hige"; height = 150.5; weight = 200.0; birthday = (9,23); blood = "B"}] = ["hoge"]
let test3 = otomeza [{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (8,23); blood = "A"};
             { name = "hige"; height = 150.5; weight = 200.0; birthday = (8,22); blood = "B"};
             { name = "huge"; height = 180.5; weight = 30.0; birthday = (9,22); blood = "A"}] = ["hoge"; "huge"]
