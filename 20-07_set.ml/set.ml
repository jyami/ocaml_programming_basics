(* 赤黒木を表す型 *)
type 'a t = 'a list

let empty = []

let singleton e = [ e ]

let rec union s1 s2 = match s1 with
  [] -> s2
  | first :: rest -> if List.exists (fun e -> e = first) s2
    then union rest s2
    else first :: (union rest s2)

let rec inter s1 s2 = match s1 with
  [] -> []
  | first :: rest -> if List.exists (fun e -> e = first) s2 
    then first :: (inter rest s2)
    else (inter rest s2)

let diff s1 s2 = 
  let unio = union s1 s2 in
    let inte = inter s1 s2 in
      let rec hojo u i = match u with
        [] -> []
        | first :: rest ->
            if List.exists (fun e -> e = first) i
            then (print_string "hit"; print_newline (); hojo rest i) 
            else (print_string "miss"; print_newline (); first :: (hojo rest i))
            in
      hojo unio inte

let mem elm s = List.exists (fun e -> e = elm) s

let length s = List.length s
