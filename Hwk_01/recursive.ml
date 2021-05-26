
(*Function odd that takes an integer input (o) and returns true if it is odd, false otherwise
  Can be done without using recursion however for the sake of the recursive exersice it was included *)

let rec odd  o=
  match o with
  | o -> if o mod 2 = 0 then false else true

(*Function euclid that takes two integer values (a and b) and returns their greatest common factor*)

let rec euclid a b =
  if a< b then euclid a (b-a)
  else if a>b then euclid (a-b) b
  else a
(*Function frac_simplify that takes a tuple with values of int values (a,b) and calls upon euclid
  function to find the greatest common factor to reduce the fractions. Returns the simplified fraction in the form
  of a tuple *)
let rec frac_simplify (a, b) =
  ((a / euclid a b), (b / euclid a b))

(* Function min_list that takes in a list of integers and returns the smallest value of that list.
 Limitations : min_list must pass in an integer list with elements (no empty lists or other list types). empty
  lists will raise a failure if passed in*)
let rec min_list (lst: int list) : int=
  match lst with
  | [] -> raise(Failure "This list is empty")
  | x::[] -> x
  | x::xs ->  let smallest= min_list xs
              in if x < smallest then x else smallest


(*Function drop that takes in a list and value (a) and drops that many values from the list.*)
let rec drop a lst =
  match lst with
  | [] -> []
  | x::xs -> if a = 1 then xs else if a =0 then lst else drop(a-1) xs
