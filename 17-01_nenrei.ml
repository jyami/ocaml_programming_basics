type nengou_t = Meiji of int
  | Taisho of int
  | Showa of int
  | Heisei of int
  | Reiwa of int

let to_seireki nengou = match nengou with
  Meiji (n) -> n + 1867
  | Taisho (n) -> n + 1911
  | Showa (n) -> n + 1925
  | Heisei (n) -> n + 1988
  | Reiwa (n) -> n + 2018

let test = Showa (46)
let test0 = to_seireki (Showa (46))

(* 誕生年と現在の年を受け取ったら年齢を返す *)
(* nenrei: nengou_t -> nengou_t -> int *)

let nenrei tanjou genzai = (to_seireki genzai) - (to_seireki tanjou)

let test1 = nenrei (Showa (46)) (Reiwa (6))
