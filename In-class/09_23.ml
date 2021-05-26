let rec find_all_with f l =
  match l with
  | [] -> []
  | x::xs ->
          let rest = find_all_with f xs
          in if f x then x::rest else rest

let rec find_all f lst =
  find_all_with (fun elem ->elem = f) lst

let rec big_nums n lst =
  find_all_with (fun elem -> elem > n) lst

let rec find_all_by eq elem lst =
  match lst with
  | [] -> []
  |x::xs when eq elem x -> x :: find_all_by eq elem xs
  |_::xs -> find_all_by eq elem xs

let rec big_strs n lst =
  find_all_by (fun s n -> String.length s > n) n lst

let rec find_all_by' eq elem lst =
  find_all_with (fun n -> eq n elem ) lst

  let rec find_all_with' elem lst =
    find_all_by(fun n -> n) elem lst

let rec drop_while lst  p =
  match lst with
  | [] -> []
  | x::xs when p x -> drop_while xs p
  | _::_ ->lst
