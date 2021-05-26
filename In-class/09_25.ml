(* The type of map is: ( 'a -> 'b) -> 'a list ->  'b list *)

let rec map f lst =
  match lst with
  | [] -> []
  | x::xs -> f x :: map f xs


(*Type of filter: ('a -> bool) -> 'a list -> 'a list = <fun> *)
let rec filter f lst =
  match lst with
  | [] -> []
  | x::xs -> if f x then x::filter f xs else filter f xs
(*let removeABC cs =
  let f c = c <> 'A' && 'B' && 'C' &&'D'
  match cs with
  | c -> class *)

(* Type of ('a -> 'a -> 'a') -> 'a -> 'a list -> 'a*)

let rec fold f base lst = match lst with
  | [] -> base
  | x::xs -> f x (fold f base xs)
