(* 目的: person_t型のリストを受け取ったら、その中から最初のA型の人のレコードをオプション型で返す。 *)
(* first_A : person_t list -> person_t option *)

type person_t = {
    blood : string
}

let rec first_A l = match l with
  [] -> None
  | first :: rest -> match first with
    {blood = b} -> if b = "A" 
      then Some (first)
      else first_A rest

let test0 = first_A [] = None
let test1 = first_A [{blood="A"}] = Some {blood="A"}
let test2 = first_A [{blood="O"}; {blood="A"}; {blood="B"}; {blood="O"}; {blood="A"}] = Some {blood="A"}
