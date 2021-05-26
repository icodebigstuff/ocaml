let length lst =
  List.fold_left (fun x _ -> x + 1) 0 lst

let andf lst =
List.fold_left (fun x a -> x && a) true lst

let orf lst =
List.fold_left (fun x a -> x || a) false lst

let is_elem x lst =
  List.fold_left(fun b a -> b || a == x ) false lst

let rev lst =
  List.fold_left(fun lst a -> a::lst) [] lst

let ascii_sum lst =
   List.fold_left (fun a b-> Char.code b + a) 0 lst

let lebowski lst =
  List.fold_left (fun accum x-> if x == '.' then accum@[','; ' '; 'd'; 'u'; 'd'; 'e'; '.']  else accum@[x]) [] lst
