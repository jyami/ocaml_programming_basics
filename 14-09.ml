type person_t = {
    name : string;
    height : float;
    weight : float;
    birthday : int * int;
    blood : string
}

let test = (fun p -> match p with {name = name} -> name) { name = "hoge"; height = 170.1; weight = 100.0; birthday = (6,4); blood = "A"} = "hoge"
