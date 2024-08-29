type person_t = {
    blood : string
}

(* person_t型リストlstを受け取ったら、bloodフィールド(血液型)で集計して最も数が多い血液型を返す *)
(* saita_ketsueki : person_t list -> string *)

let rec ketsueki_shukei lst = match lst with
  [] -> (0,0,0,0)
  | {blood=bl} :: rest ->
    let (o,a,b,ab) = ketsueki_shukei rest in
      if bl = "O" then (o+1,a,b,ab)
      else if bl = "A" then (o,a+1,b,ab)
      else if bl = "B" then (o,a,b+1,ab)
      else (o,a,b,ab+1)

let saita_ketsueki lst = match (ketsueki_shukei lst) with
  (0,0,0,0) -> ""
  | (o,a,b,ab) ->
    if o >= a && o >= b && o >= ab then "O"
    else if a >= o && a >= b && a >= ab then "A"
    else if b >= o && b >= a && b >= ab then "B"
    else "AB"

let test0 = saita_ketsueki [] = ""
let test1 = saita_ketsueki [{blood="A"}] = "A"
let test2 = saita_ketsueki [{blood="A"}; {blood="O"}; {blood="A"}; {blood="B"}; {blood="O"}; {blood="A"}] = "A"
let test2 = saita_ketsueki [{blood="A"}; {blood="O"}; {blood="A"}; {blood="O"}; {blood="O"}; {blood="A"}] = "O"
