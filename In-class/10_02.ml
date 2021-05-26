type shape = Circle of coord * float
            | Triangle of coord * coord * coord
            | Rect of sqr_desc

let isRect s = match s with
    | Rect _ -> true
    | _ -> false


(* in C : int [] (array type constructor), int * pointer type constructor

Class types in Java such as Person and ArrayList *)

let rec minList lst = match lst with
  | [] -> None
  | x::[] -> Some x
  | x::xs -> if x < minList xs
              then Some x
            else minList xs
