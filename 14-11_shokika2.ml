#use "09-09_global_ekimei_list.ml" 
#use "12-01_eki_t.ml"
#use "12-02_make_eki_list.ml"

(* eki_t型のリストと起点名を受け取ったら、起点のみsaitankyoriは0.、temae_listは始点駅名からのみなるリストを持つeki_t型リストを返す *)
(* shokika2 : eki_t list -> string -> eki_t list *)

let shokika2 lst kitenmei = List.map (fun e -> match e with 
  {namae=namae; saitan_kyori=saitan_kyori; temae_list=temae_list} ->
    if namae = kitenmei then {namae=namae; saitan_kyori=0.; temae_list=[namae]}
    else e) lst

let ekimei_list = [ 
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; shozoku="千代田線"}; 
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikouen"; shozoku="千代田線"}; 
]

let test0 = shokika2 [] "代々木公園" = []
let test1 = shokika2 (make_eki_list ekimei_list) "代々木公園" = [ 
{namae="代々木上原"; saitan_kyori=infinity; temae_list=[]}; 
{namae="代々木公園"; saitan_kyori=0.; temae_list=["代々木公園"]}; 
]
