let head lst = match lst with
  |[] -> failwith "Oh no the empty list"
  | hd::_ ->hd

let rec drop_value to_drop l =
  match l with
  | [] -> []
  |hd::tl when hd=to_drop -> drop_value to_drop tl
  |hd::tl ->hd :: drop_value to_drop tl

(* type of function: 'a * 'b *'c -> 'a) *)
let first_of_3 (x, y,z)  = x

type fraction = int * int

let add_frac  (f1:fraction) (f2:fraction) : fraction =
  match f1, f2 with
  | (n1, d1), (n2,d2) -> (n1, n2)

let rec lookup_all key m =
  match m with
  | [] -> []
  |(name,value) :: more ->
  let rest = lookup_all key more
  in if key = name then value :: rest else rest

let rec fib x =
  match x with
  | 0 -> 0
  | 1 -> 1
  | fib x-1 + fib x -2
