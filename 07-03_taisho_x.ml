(* taisho = float * float -> float * float *)

let taisho_x pair = match pair with
  (a, b) -> (a, b *. -1.)

let test1 = taisho_x (3., 1.) = (3., -1.)
