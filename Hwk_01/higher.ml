(*Function all_odds that takes in a list of integers and uses List.filter to return only the all_odds
  elements in that list *)
let all_odds = List.filter (fun x -> (x mod 2) != 0)

(*Function decrement_all that decrements all elements of an integer list by one
  by using List.map *)
let decrement_all = List.map (fun x -> (x-1))

(* Function min_fold that takes in a list of integers and returns the smallest value of that list.
 Limitations : min_fold must pass in an integer list with elements (no empty lists or other list types). empty
  lists will raise an error*)
let min_fold lst=
  if lst = [] then raise(Failure "This list is empty") else
  List.fold_left (fun a b-> if a < b then a else b) 1 lst

(*Function sum_prod takes in an integer list and returns a tuple of its sum and product *)
let sum_prod lst =
  (List.fold_left (+) 0 lst, List.fold_left ( * ) 1 lst )


(*Function partition_left takes in a function (f) and a list and  uses List.fold_left to partition a list into the
  the limitations specified by the passed in function *)
let partition_left f lst=
  List.fold_left (fun (a,b) x ->
    if f x then (x::a, b) else (a, x::b)) ([], []) (List.rev lst)

    (*Function partition_right takes in a function (f) and a list and  uses List.fold_right to partition a list into the
      the limitations specified by the passed in function *)
let partition_right f lst=
  let first second (a, b) =
    if f second then (second::a, b) else (a, second::b)
  in
  List.fold_right first lst ([], [])

(* Function map_as_fold that takes in a function (f) and a list. Chose to do it using fold_right becaue I found it difficult to do so
  via fold_left. Such as I would need to reverse the list if I chose to do it via fold_left*)
let map_as_fold f lst = List.fold_right (fun x y -> (f x)::y) lst []
