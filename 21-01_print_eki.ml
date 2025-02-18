
type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}

let rec print_stringlist l = match l with
  [] -> ()
  | first :: rest  -> print_string first; print_string ";"; print_stringlist rest

let print_eki eki = match eki with
  { namae = namae; saitan_kyori = saitan_kyori; temae_list = temae_list } -> (
    print_string "名前:";
    print_string namae;
    print_string "最短距離:";
    print_float saitan_kyori;
    print_string "手前リスト:";
    print_stringlist temae_list;
    print_newline ();
  )

let eki_list = {namae="代々木上原"; saitan_kyori=1.0; temae_list=["hoge";"hige";]}


let test1 = print_eki eki_list 
