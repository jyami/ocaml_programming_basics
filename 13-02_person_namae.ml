type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

let rec map f lst = match lst with
  [] -> []
  | first :: rest -> f first :: map f rest

let pickup_name p = match p with
  {name = n} -> n


(* 目的:person_tのリストを受け取ったら人の名前のリストを返す *)
(* person_namae : person_t list -> string list *)
let person_namae lst = map pickup_name lst

let test0 = person_namae [] = []
let test1 = person_namae [
{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"};
{ name = "hige"; height = 150.5; weight = 200.0; birthday = (12,1); blood = "B"};
{ name = "huge"; height = 180.5; weight = 30.0; birthday = (1,5); blood = "A"}
] = ["hoge"; "hige"; "huge"]
