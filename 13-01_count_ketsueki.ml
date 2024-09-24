type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

(* 目的:person_tを受け取ったら指定された血液型の人の数を返す *)
(* count_ketsueki : person_t list -> int *)
let rec count_ketsueki lst bld = match lst with
  [] -> 0
  | {blood = b} :: rest -> 
    if b = bld then 1 + count_ketsueki rest bld
    else count_ketsueki rest bld

let test0 = count_ketsueki [] "A" = 0
let test1 = count_ketsueki [
{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"};
{ name = "hige"; height = 150.5; weight = 200.0; birthday = (12,1); blood = "B"};
{ name = "huge"; height = 180.5; weight = 30.0; birthday = (1,5); blood = "A"}
] "A" = 2
