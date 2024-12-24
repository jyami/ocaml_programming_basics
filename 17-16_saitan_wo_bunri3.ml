type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}

(* 目的:eki_t型と、eki_t list型のリストを受け取ったら「最短距離最小の駅」と最短距離最小駅以外からなるリストの組を返す *)
(* saitan_wo_bunri3 : eki_t -> eki_t list -> eki_t * eki_t list *)
let saitan_wo_bunri3 kouho_first kouho_rest = match kouho_rest with
  [] -> (kouho_first, [])
  | lst -> List.fold_right
    (fun target (result, others) -> 
      if target.saitan_kyori <= result.saitan_kyori
      then (target, result :: others)
      else (result, target :: others)
    ) lst (kouho_first, [])

let test0 = saitan_wo_bunri3 {namae = "dummy"; saitan_kyori = 2.0; temae_list = []} [] = ({namae = "dummy"; saitan_kyori = 2.0; temae_list = []}, [])

let test2 = saitan_wo_bunri3 {namae = "dummy"; saitan_kyori = 2.0; temae_list = []} [
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
] = ({namae = "dummy"; saitan_kyori = 1.0; temae_list = []}, [
    {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
    {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
])

let test3 = saitan_wo_bunri3 {namae = "dummy"; saitan_kyori = 2.0; temae_list = []} [
  {namae = "dummy"; saitan_kyori = 1.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
] = ({namae = "dummy"; saitan_kyori = 1.0; temae_list = []}, [
    {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
    {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
])

let test4 = saitan_wo_bunri3 {namae = "dummy"; saitan_kyori = 1.0; temae_list = []} [
  {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
  {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
] = ({namae = "dummy"; saitan_kyori = 1.0; temae_list = []}, [
    {namae = "dummy"; saitan_kyori = 3.0; temae_list = []};
    {namae = "dummy"; saitan_kyori = 2.0; temae_list = []};
])
