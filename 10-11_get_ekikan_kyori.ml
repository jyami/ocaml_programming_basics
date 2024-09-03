#use "09-10_global_ekikan_list.ml" 

(* 漢字の駅名2つと駅間リスト受け取ったら2駅間の距離を返す *)
(* get_ekikan_kyori : string -> string -> ekikan_t lst -> float *)

let rec get_ekikan_kyori ekimei1 ekimei2 lst = match lst with
  [] -> infinity
  | first :: rest -> match first with
    {kiten = kiten; shuten = shuten; kyori = kyori} -> 
      if kiten = ekimei1 && shuten = ekimei2 then kyori
      else if kiten = ekimei2 && shuten = ekimei1 then kyori
      else get_ekikan_kyori ekimei1 ekimei2 rest

let test0 = get_ekikan_kyori "新大塚" "hige" [] = infinity
let test1 = get_ekikan_kyori "新大塚" "hige" global_ekikan_list = infinity
let test2 = get_ekikan_kyori "新大塚" "茗荷谷"  global_ekikan_list = 1.2
let test3 = get_ekikan_kyori "後楽園" "茗荷谷"  global_ekikan_list = 1.8
