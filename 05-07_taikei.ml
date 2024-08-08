(* BMI指数が18.5未満ならやせ、18.5以上25未満なら標準、25以上30未満なら肥満、30以上なら高度肥満を返す*)
(* hanbetsushiki = float->float->string *)

let bmi w h = w /. (h *. h)

let test1 = bmi 50.0 2.0 = 12.5

let taikei w h =
  if bmi w h < 18.5 then "やせ"
  else if 18.5 <= bmi w h && bmi w h < 25.0 then "標準"
  else if 25.0 <= bmi w h && bmi w h < 30.0 then "肥満"
  else"高度肥満"

let test1 = taikei 50.0 2.0 = "やせ"
