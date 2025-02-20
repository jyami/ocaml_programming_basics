let fib_array ar =
  let rec hojo n = 
    if n = 0 then
      0
    else if n = 1 then
      (ar.(0) <- 0; 0;)
    else if n = 2 then
      (ar.(0) <- 0; ar.(1) <- 1; 1;)
    else
      let sum = (hojo (n - 1) + hojo (n - 2)) in
        ar.(n-1) <- sum; sum
    in hojo (Array.length ar); ar


let test0 = fib_array [||]
let test1 = fib_array [|1;|]
let test2 = fib_array [|1;2;|]
let test3 = fib_array [|1;2;3;|]
let test4 = fib_array [|1;2;3;4;|]
let test5 = fib_array [|1;2;3;4;5;|]
let test6 = fib_array [|1;2;3;4;5;6;|]
let test7 = fib_array [|1;2;3;4;5;6;7;|]
