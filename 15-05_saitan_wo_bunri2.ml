type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}

let eki_saitan_kyori_min2 first result = if first.saitan_kyori <= result.saitan_kyori then first else result

let eki_saitan_kyori_min2 lst = match lst with
  [] -> {namae = "dummy"; saitan_kyori = infinity; temae_list = []}
  | first :: rest -> List.fold_right
    (fun target result -> 
      if target.saitan_kyori <= result.saitan_kyori 
      then target 
      else result
    ) rest first

let test0 = eki_saitan_kyori_min2 [] = {namae = "dummy"; saitan_kyori = infinity; temae_list = []}
let test1 = eki_saitan_kyori_min2 [
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
] = {namae = "dummy"; saitan_kyori = 1.0; temae_list = []}

(* 目的:eki_t list型のリストを受け取ったら「最短距離最小の駅」と最短距離最小駅以外からなるリストの組を返す *)
(* saitan_wo_bunri2 : eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri2 lst = match lst with
  [] -> ({namae = "dummy"; saitan_kyori = infinity; temae_list = []}, [])
  | first :: rest -> List.fold_right
    (fun target (result, others) -> 
      if target.saitan_kyori <= result.saitan_kyori
      then (target, result :: others)
      else (result, target :: others)
    ) rest (first, [])

let test2 = saitan_wo_bunri2 [
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
]

let test3 = saitan_wo_bunri2 [
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
]

let test4 = saitan_wo_bunri2 [
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
]
