
let twice f =
  let g x = f (f x)
  in g

let twice_twice = twice twice

let add3 x = x + 3

let test1 = add3 1 = 4
let test2 = (twice add3) 1 = 7
let test3 = (twice_twice add3) 1 = 13
