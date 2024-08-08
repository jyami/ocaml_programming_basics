(* 国語、数学、英語、理科、社会の点数を与えられたら合計点と平均点を返す *)
(* goukei_to_heikin float->float->float->float->float->(float, float) *)

let goukei k su e r sh = k +. su +. e +. r +. sh

let test1 = goukei 1.0 2.0 3.0 4.0 5.0 = 15.0

let heikin k su e r sh = goukei k su e r sh /. 5.0

let test1 = heikin 1.0 2.0 3.0 4.0 5.0 = 3.0

let goukei_to_heikin k su e r sh = (goukei k su e r sh , heikin k su e r sh)

let test1 = goukei_to_heikin 1.0 2.0 3.0 4.0 5.0 = (15.0, 3.0)
