(*Samira Osman - osman293@umn.edu *)

(*I understand that the work to be done on Quiz 1 is to be done on my
own, without any help from my classmates or others.  Discussing the
questions or solutions is an act of academic misconduct and I
understand the penalties as laid out in the syllabus for this. *)

let even x = x mod 2 = 0

(*count_evens takes in a list of integers and returns the number of even numbers within
  the list. It does this by matching the list with an empty list, if it is empty it returns 0.
  If the list is not empty it calls the even function that utilizes mod to check if the divisor of 2
  of the element in that position of the list has a remainder. If it does not it is even and the match statement
  adds one to the current count and recursivly calls count_evens until all of the list is exhausted. If
  there is a remainder nothing is added and count_evens is called again. I chose to implement this function
  the way I did (Recursive and using even) because I already had the even function written and it was more
  concise style-wise to call it and pass in a value then to re-write it within the function body. *)

let rec count_evens lst =
    match lst with
    |[] -> 0
    |x::xs -> if even x = true then 1 +count_evens xs else count_evens xs


let rec count f lst =
    match lst with
    |[] -> 0
    |x::xs -> let rest = count f xs
              in if f x then 1 + rest else rest

let count_evens_2 lst =
    count (fun x -> x mod 2 = 0) lst


(*In is_square -1 indicates that the number (num) passed in is not a square by comparing it
    to another number (x). A negative number can not be a square hence it returns -1 in the helper function . *)
let is_square num =
    let rec helper x =
      if x*x = num then x else if num<0 then -1
      else if x = 0 then -1 else helper (x-1)
    in if helper num != -1 then true else false

let count_squares lst =
    count (is_square) lst

let subtract_lst lst =
    match lst with
    | [] -> 0
    |x::[] -> x
    |x::xs -> List.fold_left(fun x y -> x - y) x xs

(* In subtract_lst I used List.fold_left because it is left-associative. The way my fold function processes
  my function is by implementing a subtraction between two list elements if there are at least two elements in the list.
  It uses the base as the first element and goes through all the rest of the elements subtracting them from the first and accumulating
  that subtracted value to be the new base until the list is exhausted. *)


let suml lst =
    let rec fold accum lst =
        match lst with
        |[] -> accum
        |x::xs -> fold (x+accum) xs
    in fold 0 lst

(*Trace: suml (1::2::4::[])
  -> fold (+) 0 (1::2::4::[])
  -> fold (+) ((+) 0 1)(2::4::[])
  -> fold (+) 1 (2::4::[])
  -> fold (+) ((+) 1 2) (4::[])
  -> fold (+) 3 (4::[])
  -> fold (+) ((+) 3 4) []
  -> fold (+) 7 []
  -> 7
  ((0+1) + 2) + 4
*)
