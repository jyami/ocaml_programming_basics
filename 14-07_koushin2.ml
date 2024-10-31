#use "12-01_eki_t.ml"
#use "10-11_get_ekikan_kyori.ml"

(*
type eki_t = {
    namae : string;
    saitan_kyori : float;
    temae_list : string list;
}
*)

(* 直前に確定した駅pと未確定の駅のリストvを受け取ったら、必要な更新処理を行った後の未確定駅のリストを返す*)
(* koushin2 : eki_t -> eki_t list -> eki_t list *)

let koushin2 p v =  let koushin1 p q = match p with
  { namae = pnamae; saitan_kyori = psaitan_kyori; temae_list = ptemae_list } -> match q with
    { namae = qnamae; saitan_kyori = qsaitan_kyori; temae_list = qtemae_list } -> 
    let kyori = get_ekikan_kyori pnamae qnamae global_ekikan_list in
      let p_keiyu_kyori = kyori +. psaitan_kyori in
        if kyori = infinity then q
        else if p_keiyu_kyori < qsaitan_kyori then
          { namae = qnamae; saitan_kyori = p_keiyu_kyori; temae_list = qnamae :: ptemae_list }
        else q
  in let koushin1Withp q = koushin1 p q
  in List.map koushin1Withp v

let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
let lst = [eki1; eki2; eki3; eki4] 
 
let test1 = koushin2 eki2 [] = [] 
let test2 = koushin2 eki2 lst = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 
