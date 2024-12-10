#use "17-02_year_t.ml"
#use "17-03_seiza_t.ml"

(* 目的：日付けを受け取ってきたら星座を返す *) 
(* seiza : year_t -> seiza_t *) 
let seiza year = match year with 
    January (d) -> if d <= 19 then Capricorus else Aquarius 
  | February (d) -> if d <= 18 then Aquarius else Pisces 
  | March (d) -> if d <= 20 then Pisces else Aries 
  | April (d) -> if d <= 19 then Aries else Taurus 
  | May (d) -> if d <= 20 then Taurus else Gemini 
  | June (d) -> if d <= 21 then Gemini else Cancer 
  | July (d) -> if d <= 22 then Cancer else Leo 
  | August (d) -> if d <= 22 then Leo else Virgo 
  | September (d) -> if d <= 22 then Virgo else Libra 
  | October (d) -> if d <= 23 then Libra else Scorpius 
  | November (d) -> if d <= 21 then Scorpius else Sagittarius 
  | December (d) -> if d <= 21 then Sagittarius else Capricorus 

  let test0 = seiza (June (4)) = Gemini
