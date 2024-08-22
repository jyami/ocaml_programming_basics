type person_t = {
    name : string;
}

(* person_t型リストlstを受け取ったら、それをnameフィールド昇順に並べたリストを返す *)
(* person_sort : person_t list -> person_t list *)

let rec insert lst p = match lst with
  [] -> [p]
  | first :: rest -> match p with
    {name = pt} -> match first with 
      {name = ft} -> if pt > ft then first :: (insert rest p)
                                  else p :: first :: rest

let rec person_sort lst = match lst with
  [] -> []
  | first :: rest -> insert (person_sort rest) first

let test0 = person_sort [] = []
let test1 = person_sort [{name="a"}] = [{name="a"}]
let test2 = person_sort [{name="ab"}; {name="a"}] = [{name="a"}; {name="ab"}]
let test3 = person_sort [{name="b"}; {name="a"}; {name="ab"}] = [{name="a"}; {name="ab"}; {name="b"}]
