(* person_t型のデータリストを受け取ったら、血液型がA型の人の数を返す *)
(* otomeza : person_t list -> int *)

type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

let rec count_ketsueki_A lst = match lst with
  [] -> 0
  | {blood = bld} :: rest -> if bld = "A"
    then 1 + count_ketsueki_A rest
    else count_ketsueki_A rest

let test0 = count_ketsueki_A [] = 0
let test1 = count_ketsueki_A [{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"}] = 1
let test2 = count_ketsueki_A [{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"};
             { name = "hige"; height = 150.5; weight = 200.0; birthday = (12,1); blood = "B"}] = 1
let test3 = count_ketsueki_A [{ name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"};
             { name = "hige"; height = 150.5; weight = 200.0; birthday = (12,1); blood = "B"};
             { name = "huge"; height = 180.5; weight = 30.0; birthday = (1,5); blood = "A"}] = 2
