(* CREDIT to CSCI2041 Course Materials Provided by Professor Van Wyk for read_file function. *)
let read_file (filename:string) : string list =
  let rec read_lines channel sofar =
    try
      let ln = input_line channel
      in read_lines channel (ln :: sofar)
    with
    | End_of_file -> sofar
    | e -> raise e
  in
  try
    let channel = open_in filename
    in
    let lines_in_reverse = read_lines channel []
    in List.rev lines_in_reverse
  with
  | e -> raise e

(* CREDIT to CSCI2041 Course Materials Provided by Professor Van Wyk for explode function. *)
  let rec explode : string -> char list = function
    | "" -> []
    | s  -> String.get s 0 :: explode (String.sub s 1 ((String.length s) - 1))

(* CREDIT to CSCI2041 Course Materials Provided by Professor Van Wyk for implode function. *)
  let rec implode : char list -> string = function
    | []    -> ""
    | c::cs -> String.make 1 c ^ implode cs


let rec get_last lst  =
    match lst with
    | [] -> []
    | x::[] -> [x]
    | x::xs -> get_last xs

let get_first lst =
    match lst with
    | [] -> []
    | x::xs -> [x]

let it_takes_two (filename: string) : (string * string) list =
    let file = read_file filename in
        let rec helper f =
            match f with
            | [] -> []
            | x::xs -> (List.fold_left (fun acc y -> if (((List.sort compare ((explode (y)))) = (List.sort compare ((get_first (explode y))@(explode x)@(get_last (explode y))))) && ((String.length y) > 3) && ((String.length x) > 3) ) then (x,y)::acc
                        else if (((List.sort compare ((explode (x)))) = (List.sort compare ((get_first (explode x))@(explode y)@(get_last (explode x))))) && ((String.length x) > 3) && ((String.length y) > 3) ) then (y,x)::acc
                        else acc) [] xs) @ (helper xs)
        in helper file
