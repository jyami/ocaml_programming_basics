type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

(* 目的:person_tを受け取ったら「xxさんの血液型はxx型です」を返す *)
(* ketsueki_hyoji : person_t -> string *)
let ketsueki_hyoji p = match p with
  {name = name; blood = blood} ->
    name ^ "さんの血液型は" ^ blood ^ "型です"

let test1 = ketsueki_hyoji { name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"} = "hogeさんの血液型はA型です"
let test2 = ketsueki_hyoji { name = "hige"; height = 150.5; weight = 200.0; birthday = (12,1); blood = "B"} = "higeさんの血液型はB型です"
let test3 = ketsueki_hyoji { name = "huge"; height = 180.5; weight = 30.0; birthday = (1,5); blood = "O"} = "hugeさんの血液型はO型です"
