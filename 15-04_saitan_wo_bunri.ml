type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}

let rec eki_saitan_kyori_min lst = match lst with
  [] -> {namae = "dummy"; saitan_kyori = infinity; temae_list = []}
  | first :: rest ->
    let cur_min = eki_saitan_kyori_min rest in
      if first.saitan_kyori <= cur_min.saitan_kyori
        then first
        else cur_min


let test0 = eki_saitan_kyori_min [] = {namae = "dummy"; saitan_kyori = infinity; temae_list = []}
let test1 = eki_saitan_kyori_min [
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
] = {namae = "dummy"; saitan_kyori = 1.0; temae_list = []}


(* 目的:eki_t list型のリストを受け取ったら「最短距離最小の駅」と最短距離最小駅以外からなるリストの組を返す *)
(* saitan_wo_bunri : eki_t list -> eki_t * eki_t list *)
let rec saitan_wo_bunri lst = match lst with
  [] -> ({namae = "dummy"; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest ->
    let cur_min, others = saitan_wo_bunri rest in
      if first.saitan_kyori <= cur_min.saitan_kyori
        then (first, rest)
        else (cur_min, first::others)

let test2 = saitan_wo_bunri [
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
]

let test3 = saitan_wo_bunri [
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
]

let test4 = saitan_wo_bunri [
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
]
