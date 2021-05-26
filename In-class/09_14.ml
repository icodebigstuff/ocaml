let rec power x y=
  if x = 0 then 1.0 else y *. power(x-1)y

let cube_v2 x = power 3 x

let gcd (x: int) (y: int) : int =
  let min = if x > y then y else x
  in
  let rec helper i =
    if x mod i =0 && y mod i = 0
    then i
    else helper (i-1)
  in
  helper min

let rec sum xs = match xs with
  | [] -> 0
  | x::rest ->x+ sum rest

let rec all xs = match xs with
  | [] -> true
  | x::rest-> x && all rest

let rec even2ways lst = match lst with
	|[] -> true
	|x::[] -> false
	|x1::x2::rest -> if x1 mod 2 =0 && x2 mod 2 =0
  then even2ways rest else false
