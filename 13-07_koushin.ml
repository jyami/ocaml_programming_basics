#use "13-06_koushin1.ml"

(* 直前に確定した駅pと未確定の駅のリストvを受け取ったら、必要な更新処理を行った後の未確定駅のリストを返す*)
(* koushin : eki_t -> eki_t list -> eki_t list *)

let gen_koushin1a p =
  let koushin1a q = koushin1 p q
    in koushin1a

let koushin p v = 
  let koushin1a = gen_koushin1a p in List.map koushin1a v 


let eki1 = {namae="池袋"; saitan_kyori = infinity; temae_list = []} 
let eki2 = {namae="新大塚"; saitan_kyori = 1.2; temae_list = ["新大塚"; "茗荷谷"]} 
let eki3 = {namae="茗荷谷"; saitan_kyori = 0.; temae_list = ["茗荷谷"]} 
let eki4 = {namae="後楽園"; saitan_kyori = infinity; temae_list = []} 
 
let lst = [eki1; eki2; eki3; eki4] 
 
let test1 = koushin eki2 [] = [] 
let test2 = koushin eki2 lst = 
 [{namae="池袋"; saitan_kyori = 3.0; temae_list = ["池袋"; "新大塚"; "茗荷谷"]}; 
  eki2; eki3; eki4] 
