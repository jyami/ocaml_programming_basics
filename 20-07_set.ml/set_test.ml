open Set

let s0 = empty
let sA = singleton "A"
let sB = singleton "B"
let sC = singleton "C"
let sD = singleton "D"

let test0 = mem "A" sA
let test1 = not (mem "B" sA)

let sAB = union sA sB
let sAC = union sA sC
let sBC = union sB sC 

let test2 = mem "A" sAB
let test3 = mem "B" sAB
let test4 = not (mem "C" sAB)
let test5 = mem "A" sAC
let test6 = mem "C" sAC
let test7 = not (mem "B" sAC)

let test8 = inter sAB sAC = sA
let test9 = diff sAB sAC = sBC

let test10 = diff (union sA sB) (union sC sD) = union sA (union sB (union sC sD))
