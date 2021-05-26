(* This file contains a few helper functions and type declarations
   that are to be used in Homework 2. *)

(* Place part 1 functions 'take', 'drop', 'length', 'rev',
   'is_elem_by', 'is_elem', 'dedup', and 'split_by' here. *)

let rec take n l = match l with
    | [] -> []
    | x::xs -> if n > 0 then x::take (n-1) xs else []

let rec drop n l = match l with
    | [] -> []
    | x::xs -> if n > 0 then drop (n-1) xs else l

(*Function Length takes a list of type a (can be any type) and returns its length as an int *)
let length (lst: 'a list) : int = List.fold_left(fun a _ -> a+1 ) 0 lst

let rev (lst: 'a list) : 'a list= List.fold_left(fun a b -> b::a) [] lst

let is_elem_by (f: 'a -> 'b -> bool) (elem: 'b)  (lst: 'a list) = List.fold_left (fun a b -> a || f b elem) false lst

let is_elem (x: 'a)  (lst: 'a list) = is_elem_by (=) x lst

let dedup (lst: 'a list) : 'a list = List.fold_right(fun a b -> if is_elem a b then b else a::b) lst []


(*function splt_by takes a function with two values of any 'a and 'b and returns a bool, a list of  'a type values, and
  another list of a generalized 'b type. it uses is_elem_by to compare elements of the seperator list to that of the
  second list to see if they have the same element and splits it the list if so. If not prepend the element to
  the sublist *)
let split_by (f: 'a -> 'b -> bool) (lst: 'b list) (split: 'a list) : 'b list list =
    let f a b =
        if (is_elem_by f a split) then []::b
            else match b with
            | [] -> []
            | x::xs ->( a::x)::xs
    in
    List.fold_right f lst [[]]



type word = char list
type line = word list

let convert_to_non_blank_lines_of_words (lst: char list) : line list =
    let break_lines x = split_by (=) x ['\n'] in
    let break_words x = split_by (=) x ['.'; '?'; ' ';',';':'; '!'; '-'; '\n'] in
    let broken = List.map break_words (break_lines lst) in
    let remove_words = List.map (List.filter ( (!=) [] )) broken in
    let remove_lines = List.filter ( (!=) [] ) remove_words in
    remove_lines

(* Some functions for reading files. *)
let read_file (filename:string) : char list option =
  let rec read_chars channel sofar =
    try
      let ch = input_char channel
      in read_chars channel (ch :: sofar)
    with
    | _ -> sofar
  in
  try
    let channel = open_in filename
    in
    let chars_in_reverse = read_chars channel []
    in Some (rev chars_in_reverse)
  with
    _ -> None


type result = OK
     | FileNotFound of string
     | IncorrectNumLines of int
     | IncorrectLines of (int * int) list
     | IncorrectLastStanza


let comparison x y =
    match x=y, x>y with
    |true,_ -> 0
    |_,true -> 1
    |_,_ -> -1

let sorting (lst: 'a list) = List.sort comparison (dedup (List.concat lst))

let incorrect_lines (lst: line list) (num: int) : (int * int) list   =
    match lst with
    |[] -> [(-1,-1)]
    |l1::[] -> [(-1,-1)]
    |l1::l2::[] -> [(-1,-1)]
    |l1::l2::l3::[] -> [(-1,-1)]
    |l1::l2::l3::l4::[] -> [(-1,-1)]
    |l1::l2::l3::l4::l5::[] -> [(-1,-1)]
    |l1::l2::l3::l4::l5::l6::rest -> let sort_first = List.sort comparison (l1@l3)
                                     in
                                     let sort_second = List.sort comparison (l2@l4)
                                     in
                                     let sort_last = List.sort comparison (l5@l6)
                                     in
                                     let one = if l1 = l2 then [(0,0)] else [(num+1, num+2)]
                                     in
                                     let two = if l3 = l4 then one else [(num+3, num+4)]
                                     in
                                     if sort_last = sort_first || sort_last = sort_second then two else [(num+5),(num+6)]

let check_s4 (last_stanza: line list) (stanzas: line list): bool =
    if sorting(last_stanza)=  sorting(stanzas) then true else false


let paradelle (filename: string): result =
    match read_file filename with
    | None -> FileNotFound filename
    | Some chars ->
    let lines =  List.map (List.map (List.map Char.lowercase_ascii)) (convert_to_non_blank_lines_of_words chars) in
        if length lines = 24 then
        let s1= incorrect_lines (take 6 lines) 0 in
            let s2= incorrect_lines (take 6 (drop 6 lines)) 6 in
            let s3= incorrect_lines (take 6 (drop 12 lines)) 12  in
            let s4 =  (take 6 (drop 18 lines)) in
            let stan1 = (take 6 lines) in
            let stan2 = (take 6 (drop 6 lines)) in
            let stan3 = (take 6 (drop 12 lines)) in
            if s1 = [(0,0)] && s2 = [(0,0)] && s3 = [(0,0)] then (if check_s4 s4 (stan1@stan2@stan3) = true  then OK else IncorrectLastStanza)
               else if s1 != [(0,0)] && s2 = [(0,0)] && s3 = [(0,0)] then IncorrectLines s1
               else if s1 = [(0,0)] && s2 != [(0,0)] && s3 = [(0,0)] then IncorrectLines s2
               else if s1 != [(0,0)] && s2 != [(0,0)] && s3 = [(0,0)] then IncorrectLines (s1@s2)
               else if s1 != [(0,0)] && s2 = [(0,0)] && s3 != [(0,0)] then IncorrectLines (s1@s3)
               else if s1 = [(0,0)] && s2 != [(0,0)] && s3 != [(0,0)] then IncorrectLines (s2@s3)
               else if s1 != [(0,0)] && s2 != [(0,0)] && s3 != [(0,0)] then IncorrectLines (s1@s2@s3)
               else IncorrectLines s3
         else IncorrectNumLines (length lines)
