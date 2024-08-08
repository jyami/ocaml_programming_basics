(* chuten = float * float -> float * float -> float * float *)

let chuten pair1 pair2 = match pair1 with
  (p1x, p1y) -> match pair2 with
    (p2x, p2y) -> ((p1x +. p2x) /. 2.0, (p1y +. p2y) /. 2.0) 

let test1 = chuten (1., 2.)  (2., 4.) = (1.5, 3.0)
