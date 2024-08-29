type person_t = {
    blood : string
}

(* person_t型リストlstを受け取ったら、bloodフィールド(血液型)で集計してその数をO,A,B,ABの順に返す *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)

let rec ketsueki_shukei lst = match lst with
  [] -> (0,0,0,0)
  | {blood=bl} :: rest ->
    let (o,a,b,ab) = ketsueki_shukei rest in
      if bl = "O" then (o+1,a,b,ab)
      else if bl = "A" then (o,a+1,b,ab)
      else if bl = "B" then (o,a,b+1,ab)
      else (o,a,b,ab+1)


let test0 = ketsueki_shukei [] = (0,0,0,0)
let test1 = ketsueki_shukei [{blood="A"}] = (0,1,0,0)
let test2 = ketsueki_shukei [{blood="A"}; {blood="O"}; {blood="A"}; {blood="B"}; {blood="O"}; {blood="A"}] = (2,3,1,0)
