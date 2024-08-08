type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

let test1 = { name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"}
let test2 = { name = "hige"; height = 150.5; weight = 200.0; birthday = (12,1); blood = "B"}
let test3 = { name = "huge"; height = 180.5; weight = 30.0; birthday = (1,5); blood = "O"}
